module Module_5_3 where

--
-- -----------------------
-- 5.3 Монада Identity
-- -----------------------
--

-- Identity - контейнер, который упаковывает любой базовый тип, не прибавляя никаких других свойств значению
-- Identity есть и в стандартной библиотеке, но тут как обычно мы напишем реализацию сами
-- Identify - довольно бесполезный контейнер, но пойдет для показания принципов работы Монад
newtype Identity a = Identity { runIdentity :: a }
  deriving (Show, Eq)

{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
-}

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity v = Identity (f v)

instance Monad Identity where
  return x = Identity x {-упаковываем передаваемое значение в контейнер-}
  Identity x >>= f = f x {-достаем значение из передаваемого контейнера и передаем это значение в стрелку Клейсли f-}


-- Для того, чтобы монады работали правильно, требуется соблюдать следующие законы:
-- 1. return a >>= k = k a
--
-- данный закон говорит что упаковывания первоначального значчения в контейнер, а потом передача данного контейнера в
-- оператор (>>=) и получение обновленного контейнера должно быть эквивалентно простому вызову стрелки Клейсли,
-- которая по первоначальному значению строила ообновленный контейнер
--
-- 2. m >>= return = m
--
-- данный закон говорит, что применение оператора (>>=) к контейнеру и передача как стрелки Клейсли функции return должно
-- быть эквивалентно самому контейнеру
-- то есть, тут просто достается значение из контейнера и обратно упаковывается в него
-- данный закон гарантирует то, что при применении оператора (>>=) старый контекст сохранится
-- например, так мы реализовали оператор (>>=) для записей лога:
{-
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xs x) f =
  let (Log ys y) = f x
  in Log (xs ++ ys) y
-}
-- мы сохраняли старые записи, хоть и работа функции над значением, упакованным в контейнер, возвращало контейнер только
-- с комментирвоанием текущего действия, но старые сообщения теряться не должны
--
-- 3. Ассоциативность
-- (m >>= k) >>= k' = m >>= (\x -> k x >>= k')
--
-- данный закон говорит, что обработка значения в контейнере первой функцией, а потом обработка получившегося контейнера другой
-- функцией эквивалентно тому, чтобы достать значение из первоначального контейнера, обработка его стрелкой Клейсли и потом
-- обработка получившегося в результате работы стрелки Клейсли контейнера второй функцией k', а потом присоединение результата
-- работы лямбда функции к первоначальному контейнеру
--
-- более строго: мы можем левоассоциативное выражение преобразовать в правоассоциативное выражение:
-- здесь - m >>= (\x -> k x >>= k') - сначала выполняется k x >>= k', и только потом к результату работы лямбда функции
-- добавляются значения из m
--
-- например, на примере Log:
--bindLog :: Log a -> (a -> Log b) -> Log b
--bindLog (Log xs x) f =
--  let (Log ys y) = f x
--  in Log (ys ++ xs ++ ys) y <- функция сделана так, чтобы добавлять новые сообщения и в начало, и в конец, в отличие от
-- правильной реализации, где они добавлялись только в конец
--
-- я буду выделять старые сообщения скобками
--
-- в левой части мы передаем контейнер с сообщениями (xs) и значением в функцию k, и получаем сообщения (ys ++ xs ++ ys)
-- далее мы передаем контейнер с этими сообщениями в функцию k', и пусть эта функция производит сообщения ys', тогда
-- новые сообщения станут такими: ys' ++ (ys ++ xs ++ ys) ++ ys'
--
-- теперь рассмотрим работу правой части:
-- сначала у нас в контейнере есть также сообщения (xs), мы применяем оператор (>>=) к этому контейнеру и лямбда функции:
-- в лямбду функцию доставляется значение из первоначального контейнера, стрелка Клейсли обрабатывает это значение и
-- производит контейнер с собщениями ys, далее этот контейнер обрабатывается функцией k', которая также производит сообщения
-- ys', теперь у нас результат работы лямбда функции стал такой: ys' ++ (ys) ++ ys'
-- теперь осталось присоединить новые сообщения к старым из первоаначального контейнера:
-- (ys' ++ (ys) ++ ys') ++ xs ++ (ys' ++ (ys) ++ ys')
--
-- результат не эквивалентен
--
-- тепреь рассмотрим работу при правильной реализации bindLog:
--bindLog :: Log a -> (a -> Log b) -> Log b
--bindLog (Log xs x) f =
--  let (Log ys y) = f x
--  in Log (xs ++ ys) y
--
-- левая часть: xs ++ ys ++ ys'
--
-- правая часть:
-- старые сообщения: xs
-- результат работы лямбда функции: k x = ys, k x >>= k' = (ys) ++ ys'
-- присоединиям результат работы лямбда функции к старым сообщениям:
-- (xs) ++ ys ++ ys'
--
-- результат эквивалентен