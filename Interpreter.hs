{-# LANGUAGE Safe #-} 

{-
   PLESE READ CAREFULLY:

   You need to replace the occurrences of "question" by your own code
   to solve the questions. If you don't attempt a question, leave it
   as a question.

   For this file to qualify for marking:

     * The option "Safe" should not be removed.
     * The given code should not be modified.
     * It must compile without errors.

   Submissions departing from these requirement will not be considered.

   Search for "question" to find what you need to do.
-}

module Interpreter where

import Unsolved
import AbstractSyntax
import IOPrime
type Storage = Identifier -> Integer

emptyStorage :: Storage
emptyStorage i = error ("Uninitialized identifier " ++ i)

update :: Identifier -> Integer -> Storage -> Storage
update i x m = m'
 where
   m' :: Storage
   m' j | i == j    = x
        | otherwise = m j

number :: Bool -> Integer
number False = 0
number True  = 1

boolean :: Integer -> Bool
boolean 0 = False
boolean _ = True

opEval :: OpName -> [Integer] -> Integer
opEval Add     [x, y] = x + y
opEval Sub     [x, y] = x - y
opEval Mul     [x, y] = x * y
opEval Div     [x, y] = x `div` y
opEval Mod     [x, y] = x `mod` y
opEval Eq      [x, y] = number(x == y)
opEval Leq     [x, y] = number(x <= y)
opEval Less    [x, y] = number(x <  y)
opEval Geq     [x, y] = number(x >= y)
opEval Greater [x, y] = number(x >  y)
opEval And     [x, y] = number(boolean x && boolean y)
opEval Or      [x, y] = number(boolean x || boolean y)
opEval Not     [x]    = number(not(boolean x))
opEval op      xs     = error ("Interpreter bug. "
                            ++ "Please contact the software maintainer. "
                            ++ "Tried to apply " ++ show op
                            ++ " to " ++ show xs)

eval :: Storage -> Expr ->  Integer
eval m (Constant x) = x
eval m (Var i)      = m i
eval m (Op o es)    = opEval o [eval m e | e <- es]

-- Questions 1-9. Implement the function run. You may leave undefined
-- the cases that you don't know how to define. You may get partial
-- marks if only some of them are correctly defined.

run :: Program -> Storage -> IO' Storage

run (i := e)          m = return(update i (eval m e) m)

run (IfElse e p q)    m 
        | boolean(eval m e) = run p m
        | otherwise         = run q m

run (If e p)          m 
        | boolean(eval m e) = run p m
        | otherwise         = return(m)

run (While e p)       m 
        | boolean(eval m e) = m''
        | otherwise         = return(m)
                where                
                        m'' = do
                                m' <- run p m 
                                x <- run (While e p) m'
                                return x

run (Block [])        m = return m

run (Block (p : ps))  m = m''
    where
        m'' = do
                m' <- run p m
                x <- run (Block ps) m'
                return x


run (Read i)          m = 
                        do 
                             l <- getLine'
                             let x = read l 
                             return (update i x m)

run (Write e)         m = 
                        do
                                let i = show(eval m e)
                                putStrLn' i
                                return m

run (Print s)         m = 
                do
                        putStrLn' s
                        return m

-- We use this for testing in the file CorrectnessTests.hs:
runIO' :: Program -> IO'()
runIO' p = fmap (\m -> ()) (run p emptyStorage)
