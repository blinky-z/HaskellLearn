module Module_5_3 where

--
-- -----------------------
-- 5.3 Монада Identity
-- -----------------------
--

-- Identity - контейнер, который упаковывает любой базовый тип, не прибавляя никаких других свойств значению
-- Identity есть и в стандартной библиотеке, но тут как обычно мы напишем реализацию сами
newtype Identity a = Identity { runIdentity :: a }
  deriving (Show, Eq)

{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
-}

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
﻿
instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity v = Identity (f v)

instance Monad Identity where
  return x = Identity x {-упаковываем передаваемое значение в контейнер-}
  Identity x >>= f = f x {-достаем значение из передаваемого контейнера и передаем это значение в стрелку Клейсли f-}
