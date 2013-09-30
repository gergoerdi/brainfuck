module Language.MovDBz.Interpreter where

import Language.MovDBz.Syntax
import Data.Array as A
import Data.Array.MArray as A
import Data.Array.IO (IOArray)
import Data.Word
import Data.Function (fix)

type Label = Int
type Reg = Int
type Cell = Word16

run :: Int -> Int -> [(Label, Stmt Reg Label)] -> [Cell] -> IO ()
run nLabs nRegs stmts mem0 = do
    let program = A.array (0, nLabs-1) stmts
    mem <- (A.newListArray (0, nRegs-1) mem0 :: IO (IOArray Int Cell))
    flip fix 0 $ \loop pc -> do
        let stmt = program ! pc
        case stmt of
            HALT -> return ()
            PRINT reg next -> do
                x <- readArray mem reg
                print x
                loop next
            MOVDBZ from to ifZero els -> do
                x <- readArray mem from
                let (y, next) = case x of
                        0 -> (0, ifZero)
                        _ -> (x-1, els)
                writeArray mem to y
                loop next


main = run 3 1 [(0, MOVDBZ 0 0 2 1), (1, PRINT 0 0), (2, HALT)] [10]
