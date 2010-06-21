{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}
module Language.Ia32.Syntax (Label(..), Reg(..), Value(..), Op(..), Directive(..), Program(..)) where

import Data.Word (Word8)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.List (intersperse)
    
newtype Label = Label Integer deriving (Enum, Show)
    
data Reg = EAX | EBX | ECX | EDX | ESP
         deriving Show

data Value = Imm Word8
           | Reg Reg
           | Deref Reg
           | Macro String
           deriving Show
                  
data Op = Inc Value
        | Dec Value
        | Move Value Value
        | Jump Label
        | Cmp Value Value
        | JumpZero Label
        | Int80
        | Push Reg
        | Pop Reg
        deriving Show                 

data Directive = Op Op
               | LabelDef Label
               deriving Show

newtype Program = Program { getDirectives :: [Directive] }

    
instance Pretty Label where
    pPrint (Label n) = text "L" <> integer n
               
instance Pretty Reg where
    pPrint EAX = text "eax"
    pPrint EBX = text "ebx"
    pPrint ECX = text "ecx"
    pPrint EDX = text "edx"
    pPrint ESP = text "esp"

instance Pretty Value where
    pPrint (Reg r) = pPrint r
    pPrint (Imm v) = text (show v)
    pPrint (Deref r) = text "byte" <+> brackets (pPrint r)
    pPrint (Macro m) = text m

asm op args = text op <+> (sep $ punctuate comma $ map pPrint args)                        
                        
instance Pretty Op where
    pPrint (Inc v) = asm "inc" [v]
    pPrint (Dec v) = asm "dec" [v]
    pPrint (Move to from) = asm "mov" [to, from]
    pPrint (Jump l) = asm "jmp" [l]
    pPrint (Cmp v v') = asm "cmp" [v, v']
    pPrint (JumpZero l) = asm "jz" [l]
    pPrint (Int80) = text "int 0x80"
    pPrint (Push r) = asm "push" [r]
    pPrint (Pop r) = asm "pop" [r]

instance Pretty Program where
    pPrint prog = vcat $ intersperse (text "") $ [prelude, text "_start" <> colon, vcat $ map toDoc (getDirectives prog), postscript]
        where prelude = nest 8 $ vcat [text "section .text",
                                       text "global _start"]

              toDoc (Op op) = nest 8 $ pPrint op
              toDoc (LabelDef l) = pPrint l <> colon
                                
              postscript = vcat $ [nest 8 $ text "section .bss",
                                   text "BUFLEN" <+> text "equ 30000",
                                   text "BUF" <+> text "resb BUFLEN"]
