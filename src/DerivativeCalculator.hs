module DerivativeCalculus where

{-
Module     : DerivativeCalculus
Description: Student project for implementing symbolic derivative 
             calculations
-}

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -            (2*X + 1) ^ 3
 -          can be encoded as
 -             Power (Add (Prod (Coef 2) X) (Coef 1)) 3
 -
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef  a
  | Sum   (MathExpr a) (MathExpr a)
  | Prod  (MathExpr a) (MathExpr a)
  | Quot  (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Integer
  | Abs   (MathExpr a)
  | Exp   (MathExpr a)
  | Log   (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef e) _ = e
eval (Sum e1 e2) v = eval e1 v + eval e2 v
eval (Prod e1 e2) v = eval e1 v * eval e2 v
eval (Quot e1 e2) v = eval e1 v / eval e2 v
eval (Power e n) v = eval e v ^^ n
eval (Abs e) v = abs (eval e v)
eval (Exp e) v = exp (eval e v)
eval (Log e) v = log (eval e v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Sum x y
  x * y         = Prod x y
  negate x      = Prod (Coef (-1)) x
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
instance Fractional a => Fractional (MathExpr a) where
  e1 / e2        = Quot e1 e2
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  exp   e = Exp e
  log   e = Log e
  sin   _ = error "sin is left un-implemented"
  cos   _ = error "cos is left un-implemented"
  tan   _ = error "cos is left un-implemented"
  asin  _ = error "asin is left un-implemented"
  acos  _ = error "acos is left un-implemented"
  atan  _ = error "atan is left un-implemented"
  sinh  _ = error "sinh is left un-implemented"
  cosh  _ = error "cosh is left un-implemented"
  tanh  _ = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  sqrt  _ = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = 1
diff (Coef e) = 0
diff (Sum e1 e2) = Sum (diff e1) (diff e2)
diff (Prod e1 e2) = Sum (Prod (diff e1) e2) (Prod (diff e2) e1)
diff (Quot e1 e2) = Quot (Sum (Prod (diff e1) e2) 
                              (Prod (Coef (-1)) (Prod (diff e2) e1))) 
                         (Power e2 2)
diff (Power e n) = Prod (Coef (fromInteger n)) 
                        (Prod (Power e (n - 1)) (diff e))
diff (Abs e) = Prod (Quot e (Abs e)) (diff e)
diff (Exp e) = Prod (Exp e) (diff e)
diff (Log e) = Quot (diff e) e

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:

 -}
prettyPrint :: (Show a) => MathExpr a -> String
prettyPrint X = "X"
prettyPrint (Coef c) = "(" ++ show c ++ ")"
prettyPrint (Sum e1 e2) = "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ ")"
prettyPrint (Prod e1 e2) = "(" ++ prettyPrint e1 ++ " * " ++ prettyPrint e2 ++ ")"
prettyPrint (Power e n) = "(" ++ prettyPrint e ++ " ^^ (" ++ show n ++ "))"
prettyPrint (Abs e) = "abs (" ++ prettyPrint e ++ ")"
prettyPrint (Exp e) = "exp (" ++ prettyPrint e ++ ")"
prettyPrint (Log e) = "log (" ++ prettyPrint e ++ ")"
