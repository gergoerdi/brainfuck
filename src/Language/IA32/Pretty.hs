module Language.IA32.Pretty () where

import Language.IA32.Syntax
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.List (intersperse)
    
instance Pretty Label where
    pPrint (Label n) = text "L" <> integer n
               
instance Pretty Reg where
    pPrint EAX = text "eax"
    pPrint EBX = text "ebx"
    pPrint ECX = text "ecx"
    pPrint EDX = text "edx"
    pPrint ESP = text "esp"
    pPrint EBP = text "ebp"

instance Pretty Value where
    pPrint (Imm v) = text (show v)
    pPrint (Macro m) = text m
    pPrint (Target t) = pPrint t

instance Pretty Target where
    pPrint (Reg r) = pPrint r
    pPrint (Deref r) = text "byte" <+> brackets (pPrint r)

instance Pretty Op where
    pPrint (Inc t)       = text "inc" <+> pPrint t
    pPrint (Dec t)       = text "dec" <+> pPrint t
    pPrint (Add t v)     = text "add" <+> pPrint t <> comma <+> pPrint v
    pPrint (Sub t v)     = text "sub" <+> pPrint t <> comma <+> pPrint v
    pPrint (Mov to from) = text "mov" <+> pPrint to <> comma <+> pPrint from
    pPrint (Jmp l)       = text "jmp" <+> pPrint l
    pPrint (Cmp v v')    = text "cmp" <+> pPrint v <> comma <+> pPrint v'
    pPrint (JmpZero l)   = text "jz" <+> pPrint l
    pPrint (Int80)       = text "int 0x80"

instance Pretty Program where
    pPrint prog = vcat $ intersperse (text "") $ [prelude, text "_start" <> colon, vcat $ map toDoc (getDirectives prog), postscript]
        where prelude = nest 8 $ vcat [text "section .text",
                                       text "global _start"]

              toDoc (Op op) = nest 8 $ pPrint op
              toDoc (LabelDef l) = pPrint l <> colon
                                
              postscript = vcat $ [nest 8 $ text "section .bss",
                                   text "BUFLEN" <+> text "equ 30000",
                                   text "BUF" <+> text "resb BUFLEN"]
