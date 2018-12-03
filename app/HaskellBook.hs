module HaskellBook where

import           Data.List
import           LibHaskellBool
main :: IO ()
s = putStrLn (host ++ ", " ++ alias)
  where
    pair = makeAlias "192.0.0.1" "google.com"
    host = fst pair
    alias = snd pair

cl = ("63we234d34", "Joshua", "a@g.com", 34)

clId = putStrLn (clientId cl)

lambdaFuncRes1 = (\x -> x * x) 5 {-lambda Func (anonymous func) with one argument-}

lambdaFuncRes2 = (\x y -> x * y) 10 4 {-lambda func with multiple arguments-}

lambdaFuncRes3 = mul 10 4
  where
    mul = \x y -> x * y

square x = x * x {-function declaration without argument types-}

argDouble = 5 :: Double

squareDoubleRes = square argDouble

argInt = 5

squareIntRes = square argInt

listLambdaFuncResult = ((\x -> x ++ " val1") "Hi!")
  where
    functions = [\x -> x ++ " val1", \x -> x ++ " val2"] {-functions is expressions, data is expressions, so we can handle funcs simply as data-}

listFuncResult = print ((head functionsResults) 5 6)
  where
    functionsResults = [prod, prod]

main = putStrLn . checkLocalHost $ "127.0.0.1" {-func composition-}

