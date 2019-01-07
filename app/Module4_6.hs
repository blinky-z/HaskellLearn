module Module4_6 where

import qualified Data.List as L
import           Prelude   hiding (lookup)

--
-- -----------------------
-- 4.6 Синонимы и обертки для типов
-- -----------------------
--

--
-- type создает синоним для типа, т.е. они полностью эквиваленты
-- например, String представлен в хаскеле представлен как [Char]
-- можно легко заменять String на [Char]
--

--
-- https://stepik.org/lesson/7602/step/3?unit=1473
-- Пусть синоним типа Endo определен следующим образом:
--
type Endo a = a -> a
--Выберите из списка типы, эквивалентные Endo (Endo Int)

-- Endo Int = Int -> Int
-- Endo (Endo Int) = Endo (Int -> Int) = (Int -> Int) -> (Int -> Int)
-- (Int -> Int) -> (Int -> Int)
--
-- допустим, функция имеет вид (Int -> Int) -> (Int -> Int)
-- тогда правда ли, что это значит следующее:
-- функция принимает первым аргументом функцию, которая принимает 1 аргумент и возвращает значение
-- можем ли мы убрать скобки вокруг первого аргумента?
-- нет, ибо тогда получится, что мы принимаем 1 аргумент типа Int, и возвращает функцию вида Int -> (Int -> Int)
-- но это нифига не так, потому что функция которая приходила первым аргументом, должна вернуть нам уже какое-то значение,
-- а не мы сами должны его получить
-- тогда смотрим дальше, на часть справа от первой стрелки -> т.е. (Int -> Int)
-- итак, мы получаем функцию типа (Int -> Int)
-- и возвращаем что? возвращаем функцию вида (Int -> Int), но ведь правда то, что эта функция принимает один аргумент типа Int
-- и возвращает значение типа Int
-- тогда мы можем убрать скобки:
-- (Int -> Int) -> Int -> Int

-- Ответ:
-- (Int -> Int) -> Int -> Int
-- (Int -> Int) -> (Int -> Int)

--
-- newtype позволяет создавать обертку над существующим типом
-- например:
newtype IntList = IList [Int]
-- newtype вводит совершенно новый тип:
-- :t IList [1,2]
--  -> IList [1,2] :: IntList
-- тогда как type всего лишь синоним для типа:
-- :t "asdasd"
--  -> ["asdasd" :: [Char]
-- это позволяет реализовать рахличающиеся интерфейсы для одних и тех же типов
-- то есть для базового [Int] будет один реализованный instance Show
-- а для IntList будет другая реализация instance Show IntList
--
-- различия в data и newtype
-- Тип, заданный с помощью newtype может иметь только один конструктор, который может иметь только один параметр
-- Это позволяет не использовать конструктор данных, так как компилятор уже знает о том, что здесь только один параметр и
-- только один конструктор,
-- по сути упаковка уже существующего типа, поэтому не надо делать pattern matching, так как он всегда будет правильным
-- то есть, если где-то в коде используется значение, упакованное в newtype, в рантайме там на самом деле будет использовать
-- это же значение, но не как упакованное в другой тип, но на высоком уровне разница есть

--
-- класс типов Monoid
-- Monoid содержит:
-- mempty - нейтральный элемент, элемент который не меняет результата применения бинарной операции
-- mappend - ассоциативную бинарную операцию
-- mconcat - свертку
{- законы:
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) <- ассоциативность бинарной операции
-}
-- структура, выполняющая все эти требования, и является моноидной
-- пример представителя тайпкласса Monoid - список:
-- instance Monoid [a] where
-- mempty = []
-- mappend = (++)
-- mconcat - уже задана стандартная реализация
--
-- так как числа являются моноидами относительно и сложения, и умножения, то потребуется делать представителя тайпкласса
-- следующим образом:
newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup (Sum a)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)
--
-- в рантайме на самом деле конструктор Sum пропадает, и вообще типы нужны только для компиляции и проверки типов, но
-- потом, после того, как проверка типов пройдена, хаскель работает просто с значениями
--
-- https://stepik.org/lesson/7602/step/7?unit=1473
-- Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.
newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    Xor x <> Xor y = Xor (x /= y)

instance Monoid Xor where
  mempty = Xor False
  Xor x `mappend` Xor y = Xor (x /= y)

-- Monoid - это абстракция, позволяет задать реализацию типов, чтобы работать с этими типами просто на основе интерфейса,
-- работая с типами, реализующими интерфейс Monoid, мы знаем что для них соблюдаются законы описанные выше

--
-- https://stepik.org/lesson/7602/step/9?unit=1473
-- Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing.
-- Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии.
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

-- что такое Maybe' a - Maybe' - абстракция над значением типа Maybe a
-- Maybe' хранит значение типа Maybe a
-- здесь, параметр a - тоже является Моноидом
-- чем отличается Моноид Maybe' a от Инстанса тайпкласса Monoid Maybe a?
-- Maybe a - является инстансом тайпкласса Monoid и делать тип данных (Maybe a) моноидным
-- какая задача у меня стоит:
-- сделать тип данных Maybe' a моноидным, где параметр a (тип данных) - тоже явлется моноидным
-- итак, Maybe' a - контейнер, который хранит значение типа Maybe a
-- а значит, он хранит значение типа Maybe a
-- для (Maybe a) уже реализован Monoid, значит что надо сделать мне:
-- Maybe a - это Just 5, Just "sada", Nothing
-- то есть я работаю на уровне Maybe все-таки, не значений, упакованных в Maybe

instance Semigroup (Maybe' a) where

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    Maybe' Nothing `mappend` m = Maybe' Nothing
    m `mappend` Maybe' Nothing = Maybe' Nothing
    Maybe' (Just m1) `mappend` Maybe' (Just m2) = Maybe' (Just (m1 `mappend` m2))
-- t1 = Maybe' (Just (Xor True)) `mappend` Maybe' (Just (Xor False)) == Maybe' {getMaybe = Just (Xor {getXor = True})}
-- t2 = Maybe' (Just (Xor False)) `mappend` Maybe' (Just (Xor True)) == Maybe' {getMaybe = Just (Xor {getXor = True})}
-- t3 = Maybe' (Just (Xor True)) `mappend` Maybe' (Just (Xor True)) == Maybe' {getMaybe = Just (Xor {getXor = False})}
-- t4 = Maybe' (Just (Xor False)) `mappend` Maybe' (Just (Xor False)) == Maybe' {getMaybe = Just (Xor {getXor = False})}
-- t1 && t2 && t3 && t4
-- t5 = Maybe' (Just [1,2]) `mappend` Maybe' (Just [3,4]) == Maybe' {getMaybe = Just [1,2,3,4]}
-- t6 = (mempty :: Maybe' [Int]) `mappend` Maybe' (Just [1,2]) == Maybe' {getMaybe = Just [1,2]}
-- t7 = Maybe' (Just [1,2]) `mappend` (mempty :: Maybe' [Int]) == Maybe' {getMaybe = Just [1,2]}
-- t8 =
-- тест: Maybe' (Just (Xor True)) `mappend` Maybe' (Just (Xor False))
-- должен вернуть True, возвращает False. 03:02 - пофиксил, закоммитил этот вариант
-- тест: Maybe' (Just (Xor False)) `mappend` Maybe' (Just (Xor True))
-- должен вернуть True, возвращает True, wat the hug !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- mconcat [Maybe' (Just (Xor True)), Maybe' (Just (Xor False))]
-- должен завершаться True
-- тест: Maybe' (Just (Xor True) `mappend` Just (Xor True)) `mappend` Maybe' (Just (Xor False))

-- разберем mempty:
-- mempty = Maybe' mempty
-- в этой реализации на любой запрос вида mempty :: Maybe' String или mempty :: Maybe' [Int]
-- выдается Maybe' {getMaybe = Nothing}
-- тут происходит что:
-- итак, нейтральным значением класса Maybe' является нейтральное значение значения упакованного в Maybe',
-- а в Maybe' у нас упаковано значение типа Maybe a, значит все время возвращается Nothing, так как именно
-- Nothing является нейтралльным значением для Maybe
-- но у нас задача другая: обеспечить работу для каждого типа данных, реализующего интерфейс Monoid
-- тогда в mempty при работе с разными типами данных должны возвращаться значения именно этих типов
-- поэтому мы переходит к следущей реализации:
--
-- mempty = Maybe' (Just mempty)
-- вообще, что такое mempty
-- mempty имеет такой тип:
--
-- mempty :: a
-- что происходит, когда мы вызываем mempty с реализацией mempty = Maybe' mempty
-- mempty :: Maybe' [Int]
-- значит mempty вернет значение типа Maybe' [Int]
-- достаточно посмотреть на вызов :t и убедиться в этом:
--  -> mempty :: Maybe' [Int] :: Maybe' [Int]
-- теперь еще раз смотрим на реализацию mempty:
-- mempty = Maybe' mempty
-- итак, что такое вообще Maybe' [Int] - это значит, что у нас есть какое-то значение типа [Int], которое упаковано в
-- контейнер Maybe, а потом было еще раз завернуто в контейнер Maybe'
-- это видно отсюда: newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
-- если a - это [Int]
-- то это превращается в: Maybe' [Int] = Maybe' { getMaybe :: Maybe [Int] }
-- снова к mempty: mempty = Maybe' mempty
-- mempty упаковывается в контейнер Maybe' и возвращается к нам,
-- а что упаковалось в Maybe'? В Maybe' упаковалось значение типа Maybe a, но для значения типа Maybe a mempty является
-- как раз так и Nothing
-- значит нам надо упаковывать в сам контейнер Maybe что-нибудь:
-- mempty = Maybe' (Just mempty)
-- то есть тут мы как бы применяем Just к mempty, а значит мы упаковываем в контейнер типа (Maybe' a) значение типа
-- (Maybe a), где значение типа a получается применением конструктора данных Just к mempty, и mempty вызовется на типе a,
-- а до этого mempty вызывался на типе Maybe a, ведь конструктор Maybe' требует параметра типа (Maybe a)

-- так, с mempty наконец понятно стало
-- теперь надо реализовать mappend
-- если к нам приходит упакованное значение типа Maybe a, упакованное в конструктор данных Just,
-- и с другой стороны Maybe тоже значение типа Maybe a, упакованное в конструктор данных Just, то мы можем применить mappend
-- к обоим значениям и получить новое, так как они тоже являются представителя класса Monoid
-- если по обоим стороным не были значения, упакованные в конструктор данных Just, то с одной стороны или по обоим сторонам
-- мы имеем Maybe' Nothing
-- если у нас одним из операндов является Maybe' Nothing, то результат должен быть Maybe' Nothing:
-- Maybe' Nothing - не нейтральный элемент
-- пусть второй элемент - это mempty, а mempty в нашей реализации упакован в (Just),
-- тогда очевидно что Maybe' Nothing `mappend` mempty = Maybe' Nothing
-- ведь для выполнения законов Моноида мы должны обеспечить:
-- x `mappend` mempty = x
--
-- если же мы передали какое-нибудь значение и mempty как два значения упакованные в конструктор данных Just,
-- то из-за того, что принимаемый параметр сам обеспечивает моноидность (из определения - Monoid a => Monoid (Maybe' a)),
-- то применение операции к ним, не изменит значения операнда не нейтрального элемента
-- то есть:
-- Maybe' (Just [1,2]) `mappend` (mempty :: (Maybe' [Int]))
--  -> Maybe' (Just [1,2]) `mappend` Maybe' {getMaybe = Just []}
--  -> Maybe' {getMaybe = Just [1,2]}
-- был передан нейтральный элемент, но значение не поменялось

-- подводя итог, всему выше проделанному
-- нашей задачей было реализовать моноидность
-- в обычной реализации Maybe, моноидность реализуется через нейтральный элемент Nothing, но так как в нашей задачей
-- было сказано реализовать моноидность, а конкретно нейтральный элемент, НЕ через Nothing,
-- то мы реализовали этот самый нейтральный элемент через нейтральный элемент типа a - Maybe' a
-- то есть, например, [Int] - [], String - ""
-- мы просто берем и упаковываем нейтральный элемент типа, реализующего интерфейс Monoid
-- ведь у нас такое объявление: instance Monoid a => Monoid (Maybe' a)
-- так как тип a - представитель Monoid,
-- это позволяет нам легко реализовывать моноидность:
-- Maybe' (Just m1) `mappend` Maybe' (Just m2) = Maybe' (Just (m1 `mappend` m2)) <- здесь, если один из операндов является
-- нейтральным элементом,
-- то, поскольку оба элемента m1 и m2 моноидны и одного типа, это позволяет нам применять операцию `mappend`, реализованную для
-- этого типа на самих значениях, упаковынных в Just
-- например:
-- Maybe' (Just [1,2]) `mappend` (mempty :: (Maybe' [Int]))
-- mempty :: (Maybe' [Int]) - это Maybe' {getMaybe = Just []}
-- мы берем и делаем так: Maybe' (Just [1,2] ++ []) <- результат не меняется
-- второй случай покрывается тоже, это когда одно из значений, упакованное в Maybe', не было упаковано в конструктор Just,
-- а было создано с помощью конструктора Nothing, т.е. Maybe' Nothing
-- но и тогда сохраняется моноидность:
-- так как mempty у нас упаковано в Just, то применение операции mappend к Maybe' Nothing и mempty, например:
-- Maybe' Nothing `mappend` (mempty :: Maybe' (Maybe [Int]))
-- не должно изменять значения НЕ нейтрального элемента, а значит остается Maybe' Nothing
-- итак, у нас есть моноидность в обоих случаях, когда было передано Nothing, завернутое в Maybe', и когда было передано
-- значение, завернутое в Just и в Maybe'

--
-- https://stepik.org/lesson/7602/step/10?unit=1473
-- Ниже приведено определение класса MapLike типов, похожих на тип Map.
-- Определите представителя MapLike для типа ListMap, определенного ниже как список пар ключ-значение.
-- Для каждого ключа должно храниться не больше одного значения. Функция insert заменяет старое значение новым, если ключ уже содержался в структуре.
--import Prelude hiding (lookup)
--import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList []         = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup k (ListMap []) = Nothing
  lookup k (ListMap (x:xs)) | fst x == k = Just (snd x)
                            | otherwise = lookup k (ListMap xs)
  insert k v map = insertHelper (k,v) map (ListMap [])
    where
      insertHelper p@(k,v) (ListMap []) (ListMap produceMap) = ListMap (reverse $ p:produceMap)
      insertHelper p@(k,v) (ListMap (x:xs)) (ListMap produceMap) | k == fst x = ListMap (reverse produceMap ++ p:xs)
                                                                 | otherwise = insertHelper p (ListMap xs) (ListMap (x:produceMap))
  delete k map = deleteHelper k map (ListMap [])
    where
      deleteHelper k (ListMap []) (ListMap produceMap) = ListMap $ reverse produceMap
      deleteHelper k (ListMap (x:xs)) (ListMap produceMap) | k == fst x = ListMap (reverse produceMap ++ xs)
                                                           | otherwise = deleteHelper k (ListMap xs) (ListMap (x:produceMap))

--
-- В хаскеле существуют эндоморфизмы
-- они позволяют упаковывать функции, вида a -> a
-- newtype Endo a = Endo { appEndo :: a -> a }


