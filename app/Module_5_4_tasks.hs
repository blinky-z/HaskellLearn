module Module_5_4_tasks where

import           Data.Char (isDigit)


--
-- https://stepik.org/lesson/8439/step/4?unit=1574
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> Maybe Token
asToken s | isNumber s = Just (Number $ read s)
          | s == "+" = Just Plus
          | s == "-" = Just Minus
          | s == "(" = Just LeftBrace
          | s == ")" = Just RightBrace
          | otherwise = Nothing
  where
    isNumber [] = True
    isNumber (x:xs) | isDigit x = isNumber xs
                    | otherwise = False

-- без использования монодических вычислений
tokenize :: String -> Maybe [Token]
tokenize input = helper (words input) []
  where
    helper [] res = Just (reverse res)
    helper (x:xs) res =
      case asToken x of
        Just x  -> helper xs (x:res)
        Nothing -> Nothing

-- монодические вычисления, функция из стандартной библиотеки
tokenize' :: String -> Maybe [Token]
tokenize' input =
  let xs = words input
  in sequence (map asToken xs)
-- на самом деле sequence полностью эквивалентен реализации ниже
-- нельзя использовать sequence не понимая как работает код ниже в do-нотации
--sequence ms = foldr k (return []) ms
--  where
--    k m m' = do { x <- m; xs <- m'; return (x:xs) }


-- монодические вычисления, реализация полностью повторяет то как работала бы функция sequence
tokenize'' input = foldr f (return []) (words input)
  where
    word `f` list = do
      x <- asToken word
      xs <- list
      return (x:xs)

-- можно легко переписать данный код не в do-нотации по правилам трансляции:
word `f` list  = asToken word >>= (\x -> list >>= (\xs -> return (x:xs)))

-- приведу реализацию инстанса Maybe для Монад, для разбора ниже
--instance Monad Maybe where
--  return x = Just x
--
--  (Just x) >>= k = k x
--  Nothing >>= _ = Nothing
--
--  (Just _) >> m = m
--  Nothing >> _ = Nothing
--

-- как работает код в do-нотации функции f:
--
-- пусть вызов был таким:
-- tokenize'' "1 + abc"
-- посмотрим, как развертывалась функция:
-- "1" `f` ("+" `f` ("abc" `f` init))
-- "1" `f` ("+" `f` ("abc" `f` Just []))
-- "1" `f` ("+" `f` ("abc" `f` Just []))
-- посмотрим на работу функции f над ("abc" `f` Just []):
-- x = Nothing
-- а значит вся следующая цепочка вычислений будет равна Nothing, так как
-- Nothing >>= _ = Nothing
-- то есть, asToken word >>= (\x -> list >>= (\xs -> return (x:xs)))
--  -> Nothing
-- но что если слева от результата вычисления с конца появляется корректным токеном?
-- то есть: "+" `f` Nothing
-- тогда работа такова:
-- asToken word >>= (\x -> list >>= (\xs -> return (x:xs)))
-- -> (\x -> list >>= (\xs -> return (x:xs))) Plus
-- -> (list >>= (\xs -> return (x:xs)))
-- -> (Nothing >>= (\xs -> return (x:xs)))
-- -> Nothing
--
-- то есть видно, что вообще, x нам нужен только в случае, если и слева и справа стоят правильные токены, тогда эти токены
-- упаковываются в return, то есть Just (x:xs)
-- x и xs - это токены, распакованные из монад Maybe
--
-- важно понимать данный код в do-нотации как цепочку вычислений
-- то есть, если думать не как о преобразовании по правилам трансляции и просто посмотреть на данную инструкцию:
-- x <- asToken word
-- x стал равен Nothing
-- но тогда ведь вся цепочка вычислений становится равной Nothing!
-- так как e1 ; e2 ...
-- здесь будет равна Nothing >> _ = Nothing
-- и дальнейшие вычисления вообще не будут вычисляться, и результат станет сразу равен Nothing
-- можно сказать, что Nothing прерывает вычисления

-- в случае, если все токены правильные, работа очвидна
-- ошибок не будет, и return (x:xs) будет генерировать монаду Maybe, в которую упакованы токены
-- x - токен слева от f
-- xs - результат работы справа, тоже токены

--
-- https://stepik.org/lesson/8439/step/6?unit=1574
--Тип Board и функция nextPositions заданы, реализовывать их не нужно
data Board

--nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
--nextPositionsN b 0 pred = if pred b then return b else []
--nextPositionsN b n pred = if n < 0 then [] else
--  do
--    p <- nextPositions b
--    d <- nextPositionsN p (n - 1) pred
--    return d

-- для разбора представим что, data Board = Board Int
-- и nextPositions (Board x) = map Board [x + 1, x - 1]
--
-- опять же, цепочка вычислений
-- с каждого элемента из p начинаются вычисления лямбда функции, куда передается элемент из p и в этой лямбде
-- вызывается функция nextPositionsN p (n - 1) pred
-- далее из каждой функции вызываются снова return d, то есть по сути можно было бы вообще не писать
-- такой код:
{-
    d <- nextPositionsN p (n - 1) pred
    return d
-}
-- а просто написать:
{-
nextPositionsN p (n - 1) pred
-}
-- но не важно, главное понять что будет происходить на последнем шаге, когда n = 1
-- там вызывается для каждого элемента p (nextPositionsN p 0 pred) и этот элемент сохраняется в d
-- как мы помним, реализация для списков такова, что применяется map к каждому элементу
-- например, если в з было [Board 4, Board 6], то потом после применения с n = 1 лямбда функции в нем все элементы
-- заменятся на результаты return d
-- то есть [[Board 4], [Board 6]]
-- но ведь у нас есть еще concat, и это превращается в [Board 4, Board 6]
-- таких цепочек вызова будет много, сейчас мы только рассмотрели, когда исходная функция вызывается на n = 1
-- но на самом деле, от каждого элемента будет порождаться еще 2 цепочки, то есть один элемент заменяться на список из двух
-- Board
-- например: [Board 4, Board 5] -> [[Board 3, Board 5], [Board 4, Board 6]]

--
-- https://stepik.org/lesson/8439/step/6?unit=1574
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = if x <= 0 then [] else
  do
    b <- [1..x]
    a <- [1..(b - 1)]
    c <- [1..x]
    True <- return (a^2 + b^2 == c^2)
    return (a, b, c)