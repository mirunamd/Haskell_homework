module Optimization where

import Data.Maybe
import Data.List

import Unsolved
import AbstractSyntax
import Interpreter

type OptStorage = Identifier -> Maybe Integer

emptyOptStorage :: OptStorage
emptyOptStorage i = Nothing

optUpdate :: Identifier -> Expr -> OptStorage -> OptStorage
optUpdate i e m = case e of
                  Constant x -> m' x
                  _          -> m''
 where
   m' x j | i == j    = Just x
          | otherwise = m j
   m'' j  | i == j    = Nothing
          | otherwise = m j

deleteVar :: Identifier -> OptStorage -> OptStorage
deleteVar i m j | i == j    = Nothing
                | otherwise = m j

deleteVars :: [Identifier] -> OptStorage -> OptStorage
deleteVars [] m = m
deleteVars (i:is) m = deleteVars is (deleteVar i m)


fromConstant :: Expr -> Integer
fromConstant (Constant x) = x
fromConstant _            = error "program bug"

isConstant :: Expr -> Bool
isConstant (Constant x) = True
isConstant _            = False

updatedVariables, updatedVariables'  :: Program -> [Identifier]
updatedVariables                 = nub . updatedVariables'
updatedVariables' (i := e)       = [i]
updatedVariables' (IfElse e p q) = updatedVariables' p ++ updatedVariables' q
updatedVariables' (If e p)       = updatedVariables' p
updatedVariables' (While e p)    = updatedVariables' p
updatedVariables' (Block ps)     = concat(map updatedVariables' ps)
updatedVariables' (Read i)       = [i]
updatedVariables' (Write e)      = []
updatedVariables' (Print s)      = []


-- You don't need to use the above functions. But you may find them
-- useful. Indeed, we use them in our sample solution.

optExpr  :: OptStorage -> Expr -> Expr
optExpr m e = case e of
        Constant i -> e
        Var i -> case m i of
                        Nothing -> e
                        Just x -> Constant x

        Op Or ([e1, e2]) -> exor (optExpr m e1) (optExpr m e2) 

        Op And [e1, e2] -> exand (optExpr m e1) (optExpr m e2) 
         
        Op Eq [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "eq"

        Op Leq [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "leq"
  
        Op Less [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "less"
   
        Op Geq [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "geq"
   
        Op Greater [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "gt"

        Op Add [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "add"

        Op Sub [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "sub"
 
        Op Mul [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "mul"
        
        Op Div [e1, e2]  -> ex (optExpr m e1) (optExpr m e2) "div"
  
        Op Mod [e1, e2] -> ex (optExpr m e1) (optExpr m e2) "mod"
   
        Op Not [e1] -> enot (optExpr m e1)


toInt :: Bool -> Integer
toInt b 
        | b = 1
        | otherwise = 0 

exor :: Expr -> Expr -> Expr
exor e1 e2 
        | b1 && b2 =  Constant (toInt(boolean(fromConstant e1) || boolean(fromConstant e2)))
        | b1 = if boolean(fromConstant e1) then Constant 1
                else Op Or [e1, e2]  
        | b2 = if boolean(fromConstant e2) then Constant 1
                else Op Or [e1, e2]     
        | otherwise = Op Or [e1, e2]     
        where
                        b1 = isConstant e1
                        b2 = isConstant e2 

exand :: Expr -> Expr -> Expr
exand e1 e2 
        | b1 && b2 =  Constant (toInt(boolean(fromConstant e1) || boolean(fromConstant e2)))
        | b1 = if boolean(fromConstant e1) == False then Constant 0
                else e2  
        | b2 = if boolean(fromConstant e2) == False then Constant 0
                else e1    
        | otherwise = Op And [e1, e2]     
        where
                        b1 = isConstant e1
                        b2 = isConstant e2 

ex :: Expr -> Expr -> String -> Expr
ex e1 e2 s 
        | b1 && b2 = case s of
                "and" ->  Constant (toInt(boolean(fromConstant e1) && boolean(fromConstant e2)))
                "eq" ->  Constant (toInt(boolean(fromConstant e1) == boolean(fromConstant e2)))
                "leq" ->  Constant (toInt((fromConstant e1) <= (fromConstant e2)))
                "less" -> Constant (toInt((fromConstant e1) < (fromConstant e2)))
                "geq" -> Constant (toInt((fromConstant e1) >= (fromConstant e2)))
                "gt" -> Constant (toInt((fromConstant e1) > (fromConstant e2)))
                "add" -> Constant ((fromConstant e1) + (fromConstant e2))
                "sub" -> Constant ((fromConstant e1) - (fromConstant e2))
                "mul" -> Constant ((fromConstant e1) * (fromConstant e2))
                "div" -> Constant (floor(fromInteger(fromConstant e1) / fromInteger(fromConstant e2)))
                "mod" -> Constant ((fromConstant e1) `mod` (fromConstant e2))
        | otherwise = case s of 
                "and" ->  Op And [e1, e2]
                "eq" ->  Op Eq [e1, e2]
                "leq" ->  Op Leq [e1, e2]
                "less" -> Op Less [e1, e2]
                "geq" -> Op Geq [e1, e2]
                "gt" -> Op Greater [e1, e2]
                "add" -> Op Add [e1, e2]
                "sub" -> Op Sub [e1, e2]
                "mul" -> Op Mul [e1, e2]
                "div" -> Op Div [e1, e2]
                "mod" -> Op Mod [e1, e2]
                where
                        b1 = isConstant e1
                        b2 = isConstant e2

enot :: Expr -> Expr
enot e1 
        | isConstant e1 = Constant (toInt(not (boolean (fromConstant e1))))
        | otherwise = Op Not [e1]

optProgram  :: Program -> OptStorage -> (Program, OptStorage)

optProgram = question "optimize program"

-- This is what we are really interested in in practice:  
  
optProgram' :: Program -> Program
optProgram' p = fst(optProgram p emptyOptStorage)