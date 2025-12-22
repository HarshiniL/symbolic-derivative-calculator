module Main where
import DerivativeCalculus

main  :: IO()
main = do
  putStrLn "Welcome to the interactive symbolic derivative calculator"
  putStrLn "Enter expression or type 'quit' to exit"
  line <- getLine
  if line == "quit" then do
      putStrLn "Exiting..."
  else do
      let expr = read line :: MathExpr Double
      putStrLn ("Expression: " ++ prettyPrint expr)
      putStrLn ("Derivative: " ++ prettyPrint (diff expr))
      putStrLn "Enter value for X: "
      xLine <- getLine
      let xValue = read xLine :: Double
      putStrLn ("Evaluated expression: " ++ show (eval expr xValue))
