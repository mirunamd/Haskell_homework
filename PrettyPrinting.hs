module PrettyPrinting where


import Unsolved
import AbstractSyntax

spaces n = take n (repeat ' ')
  
ppExpr :: Expr -> String
ppExpr e = case e of
  Constant i -> (show i)
  Var i -> i

  Op Or ([e1, e2]) -> (ppExpr e1) ++ " || " ++ (ppExpr e2)

  Op And [e1, e2] 
    | bAnd e1 && bAnd e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " && " ++ "(" ++ (ppExpr e2) ++ ")"
    | bAnd e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " && " ++ (ppExpr e2)
    | bAnd e2 -> (ppExpr e1) ++ " && " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " && " ++ (ppExpr e2)
         
  Op Eq [e1, e2] 
    | bEq e1 && bLGE e2 ->  "(" ++ (ppExpr e1) ++ ")" ++ " == " ++ "(" ++ (ppExpr e2) ++ ")"
    | bEq e1 ->  "(" ++ (ppExpr e1) ++ ")" ++ " == " ++ (ppExpr e2) 
    | bLGE e2 ->  (ppExpr e1) ++ " == " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " == " ++ (ppExpr e2) 

  Op Leq [e1, e2] 
    | bLGE e1 && bAS e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " <= " ++ "(" ++ (ppExpr e2) ++ ")"
    | bLGE e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " <= " ++ (ppExpr e2) 
    | bAS e2 ->  (ppExpr e1) ++ " <= " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " <= " ++ (ppExpr e2) 
  
  Op Less [e1, e2] 
    | bLGE e1 && bAS e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " < " ++ "(" ++ (ppExpr e2) ++ ")"
    | bLGE e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " < " ++ (ppExpr e2) 
    | bAS e2 ->  (ppExpr e1) ++ " < " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " < " ++ (ppExpr e2) 
   
  Op Geq [e1, e2] 
    | bLGE e1 && bAS e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " >= " ++ "(" ++ (ppExpr e2) ++ ")"
    | bLGE e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " >= " ++ (ppExpr e2) 
    | bAS e2 ->  (ppExpr e1) ++ " >= " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " >= " ++ (ppExpr e2) 
   
  Op Greater [e1, e2] 
    | bLGE e1 && bAS e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " > " ++ "(" ++ (ppExpr e2) ++ ")"
    | bLGE e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " > " ++ (ppExpr e2) 
    | bAS e2 ->  (ppExpr e1) ++ " > " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " > " ++ (ppExpr e2) 

  Op Add [e1, e2]  -- assoc
    | bAS e1 && (bAS e2 || sub e2) -> "(" ++ (ppExpr e1) ++ ")" ++ " + " ++ "(" ++ (ppExpr e2) ++ ")"
    | bAS e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " + " ++ (ppExpr e2) 
    | (bAS e2 || sub e2) -> (ppExpr e1) ++ " + " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " + " ++ (ppExpr e2) 

  Op Sub [e1, e2] 
    | bAS e1 && bMDM e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " - " ++ "(" ++ (ppExpr e2) ++ ")"
    | bAS e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " + " ++ (ppExpr e2) 
    | bMDM e2 -> (ppExpr e1) ++ " - " ++ "(" ++ (ppExpr e2) ++ ")"   
    | otherwise -> (ppExpr e1) ++ " - " ++ (ppExpr e2)   
 
  Op Mul [e1, e2] -- assoc
    | bMDM e1 && (bMDM e2 || mul e2) -> "(" ++ (ppExpr e1) ++ ")" ++ " * " ++ "(" ++ (ppExpr e2) ++ ")"
    | bMDM e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " * " ++ (ppExpr e2)
    | (bMDM e2 || mul e2) -> (ppExpr e1) ++ " * " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " * " ++ (ppExpr e2)
 
  Op Div [e1, e2] 
    | bMDM e1 && bN e2 -> "(" ++ (ppExpr e1) ++ ")" ++ " / " ++ "(" ++ (ppExpr e2) ++ ")"
    | bMDM e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " / " ++ (ppExpr e2)
    | bN e2 -> (ppExpr e1) ++ " / " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " / " ++ (ppExpr e2)
  
  Op Mod [e1, e2]
    | bMDM e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " % " ++ "(" ++ (ppExpr e2) ++ ")"
    | bMDM e1 -> "(" ++ (ppExpr e1) ++ ")" ++ " % " ++ (ppExpr e2)
    | bN e2 -> (ppExpr e1) ++ " % " ++ "(" ++ (ppExpr e2) ++ ")"
    | otherwise -> (ppExpr e1) ++ " % " ++ (ppExpr e2)
   
  Op Not [e1] 
    | bN e1 -> "! (" ++ (ppExpr e1) ++ ")"
    | otherwise -> "! " ++ (ppExpr e1)

mul :: Expr -> Bool
mul e = case e of
    Op Div [e1, e2] -> True
    Op Mod [e1, e2] -> True
    _ -> False

sub :: Expr -> Bool
sub e = case e of
    Op Sub [e1, e2] -> True
    _ -> False

bAnd :: Expr -> Bool
bAnd e = case e of
    Op Or [e1, e2] -> True
    _ -> False

bEq :: Expr -> Bool
bEq e = case e of
    Op And [e1, e2] -> True
    _ -> bAnd e

bLGE :: Expr -> Bool -- less, greater, equal than
bLGE e = case e of
    Op Eq [e1, e2] -> True
    _ -> bEq e

bAS :: Expr -> Bool -- add, sub
bAS e = case e of
    Op Leq [e1, e2] -> True   
    Op Less [e1, e2] -> True 
    Op Geq [e1, e2] -> True
    Op Greater [e1, e2] -> True
    _ -> bLGE e

bMDM :: Expr -> Bool -- multi, div, mod
bMDM e = case e of
    Op Add [e1, e2] -> True   
    Op Sub [e1, e2] -> True
    _ -> bAS e

bN :: Expr -> Bool -- not
bN e = case e of
    Op Mul [e1, e2] -> True   
    Op Div [e1, e2] -> True  
    Op Mod [e1, e2] -> True    
    _ -> bMDM e 
   

ppProgram :: Program -> (Int -> String)
ppProgram p n = case p of

    i := e -> (spaces n) ++ i ++ " := " ++ (ppExpr e) ++ ";\n"
    Block xs -> (spaces n) ++ "{\n" ++ block xs (n) ++ (spaces n) ++ "}\n"
    While e pr -> (spaces n) ++ "while (" ++ ppExpr e ++ ")\n" ++ (ppProgram pr (n + 2))
    If e pr -> (spaces n) ++ "if (" ++ ppExpr e ++ ")\n" ++ (ppProgram pr (n+2))
    IfElse e pr1 pr2 -> (spaces n) ++ "if (" ++ ppExpr e ++ ")\n" ++ (ppProgram pr1 (n + 2)) ++ (spaces n) ++"else\n"++ (ppProgram pr2 (n + 2))
    Read i -> (spaces n) ++ "read " ++ i ++ ";\n"
    Write expr -> (spaces n) ++ "write " ++ ppExpr expr ++ ";\n"
    Print str -> (spaces n) ++ "print " ++ str ++ ";\n"

block :: [Program] -> (Int -> String)
block bl n = case bl of
    [] -> ""
    [p] -> (ppProgram p (n + 2))
    (p : ps) -> (ppProgram p (n + 2)) ++ (block ps (n))
    
     


