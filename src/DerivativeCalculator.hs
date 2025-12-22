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
  | Power (MathExpr a) a
  | Abs   (MathExpr a)
  | Exp   (MathExpr a)
  | Log   (MathExpr a)
  | Sin   (MathExpr a)
  | Cos   (MathExpr a)
  | Tan   (MathExpr a)
  | Asin   (MathExpr a)
  | Acos   (MathExpr a)
  | Atan   (MathExpr a)
  | Sinh   (MathExpr a)
  | Cosh   (MathExpr a)
  | Tanh   (MathExpr a)
  | Asinh   (MathExpr a)
  | Acosh   (MathExpr a)
  | Atanh   (MathExpr a)
  | Sqrt   (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates an expression of MathExpr at a given value. Parameters
      are e (the expression) and v (the value to substitute for X)
      This function returns the numeric value of the expression.
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
eval (Sin e) v = sin (eval e v)
eval (Cos e) v = cos (eval e v)
eval (Tan e) v = tan (eval e v)
eval (Asin e) v = asin (eval e v)
eval (Acos e) v = acos (eval e v)
eval (Atan e) v = atan (eval e v)
eval (Sinh e) v = sinh (eval e v)
eval (Cosh e) v = cosh (eval e v)
eval (Tanh e) v = tanh (eval e v)
eval (Asinh e) v = asinh (eval e v)
eval (Acosh e) v = acosh (eval e v)
eval (Atanh e) v = atanh (eval e v)
eval (Sqrt e) v = sqrt (eval e v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Defines basic operations for MathExpr, like addition and multiplication.
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
 -   Defines fractional operations for MathExpr.
 -}
instance Fractional a => Fractional (MathExpr a) where
  e1 / e2        = Quot e1 e2
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -     Defines floating point operations for MathExpr, like the value
       of pi. However, most trigonometric and hyperbolic functions will
       raise an error.
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  exp   e = Exp e
  log   e = Log e
  sin   e = Sin e
  cos   e = Cos e
  tan   e = Tan e
  asin  e = Asin e
  acos  e = Acos e
  atan  e = Atan e
  sinh  e = Sinh e
  cosh  e = Cosh e
  tanh  e = Tanh e
  asinh e = Asinh e
  acosh e = Acosh e
  atanh e = Atanh e
  sqrt  e = Sqrt e

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    If e is an expression of type (MathExpr a), diff e is the
      result of symbolically differentiating e using the differential rules.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1
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
diff (Sin e) = Prod (Cos e) (diff e)
diff (Cos e) = Prod (Prod (Coef (-1)) (Sin e)) (diff e)
diff (Tan e) = Quot (diff e) (Power (Cos e) 2)
diff (Asin e) = Quot (diff e) (Sqrt (Sum (Coef 1) (Prod (Coef (-1)) (Power e 2))))
diff (Acos e) = Prod (Coef (-1)) (Quot (diff e) (Sqrt (Sum (Coef 1) (Prod (Coef (-1)) (Power e 2))))))
diff (Atan e) = Quot (diff e) (Sum (Coef 1) (Power e 2))
diff (Sinh e) = Prod (Cosh e) (diff e)
diff (Cosh e) = Prod (Sinh e) (diff e)
diff (Tanh e) = Quot (diff e) (Power (Cosh e) 2)
diff (Asinh e) = Quot (diff e) (Sqrt (Sum (Power e 2) (Coef 1)))
diff (Acosh e) = Quot (diff e) (Sqrt (Sum (Power e 2) (Coef (-1))))
diff (Atanh e) = Quot (diff e) (Sum (Coef 1) (Prod (Coef (-1)) (Power e 2))))
diff (Sqrt e) = diff (Power e 0.5)

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -     If e is an expression of type (MathExpr a), prettyPrint e
       will create a String representation of e.
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
prettyPrint (Sin e) = "sin (" ++ prettyPrint e ++ ")"
prettyPrint (Cos e) = "cos (" ++ prettyPrint e ++ ")"
prettyPrint (Tan e) = "tan (" ++ prettyPrint e ++ ")"
prettyPrint (Asin e) = "arcsin (" ++ prettyPrint e ++ ")"
prettyPrint (Acos e) = "arccos (" ++ prettyPrint e ++ ")"
prettyPrint (Atan e) = "arctan (" ++ prettyPrint e ++ ")"
prettyPrint (Sinh e) = "sinh (" ++ prettyPrint e ++ ")"
prettyPrint (Cosh e) = "cosh (" ++ prettyPrint e ++ ")"
prettyPrint (Tanh e) = "tanh (" ++ prettyPrint e ++ ")"
prettyPrint (Asinh e) = "arcsinh (" ++ prettyPrint e ++ ")"
prettyPrint (Acosh e) = "arccosh (" ++ prettyPrint e ++ ")"
prettyPrint (Atanh e) = "arctanh (" ++ prettyPrint e ++ ")"
prettyPrint (Sqrt e) = "sqrt (" ++ prettyPrint e ++ ")"
