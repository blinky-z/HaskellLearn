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


-- в случае, если все токены правильные, работа очвидна
-- ошибок не будет, и return (x:xs) будет генерировать монаду Maybe, в которую упакованы токены
-- x - токен слева от f
-- xs - результат работы справа, тоже токены