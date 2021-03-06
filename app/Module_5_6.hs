module Module_5_6 where

--
-- -----------------------
-- 5.6 Монада Reader
-- -----------------------
--

-- Монада Reader позволяет во время монадичных вычислений обращаться к какому-то константному окружению и считывать оттуда
-- какое-то значение (Environment monad)

-- тип этой монады - ((->) e)
-- монада Reader позволяет строить цепочку вычислений внутри определенного окружения, и окружение применяется неявно ко
-- всем функциям


-- реализуем инстанс Монады для Reader
-- пока что напишем простую монаду Reader, как через частично примененную стрелку, потом напишем настояющую монаду Reader
-- но на самом деле, все заключается именно в этой частично примененной стрелке, так что последующее написание Монады будет
-- легким

{-
instance Monad ((->) e) where
  return :: a -> m a
  return : a -> (e -> a)
  return x = \_ -> x
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
  m >>= k = \e -> k (m e) e
-}
-- как работает функция return:
-- что должен делать оператор return? Упаковывать значение в контейнер, и возвращать этот Контейнер
-- соотвественно, нам надо построить монаду Return
-- мы просто берем и принимает какое-то окружение, игнорируем его, и в итоге возвращается Монада типа (e -> a)
-- так как в return мы написали лямбда функцию, принимающую аргумент, здесь окружение
-- эта функция нужна, чтобы возвращать из монады какое-то значение, ведь именно этого мы добиваемся от монадических вычислений
-- тогда, когда мы упаковываем что-то в Монаду Reader, как мы и говорили раньше, когда мы запускаем вычисление, к функции
-- будет передаваться окружение, это окружение будет игнорировано и просто возвратится значение, хранящееся в Монаде

-- как работает оператор (>>=):
--
-- так как оператор (>>=) должен строить еще одно монадическое вычислениие, тогда
-- значит он строит еще одну монаду, где будет лежать новое значение
-- первое вычисление - это (m e), то есть доставание значения из переданной монады m
-- второе вычисление - обработка значения стрелкой Клейсли - k (m e), и построение новой Монады - (a -> (e -> b))
-- итак, на инструкции k (m e) мы построили Монаду Reader (e -> b), к которой нужно применить окружение, чтобы получить
-- конечное вычисление
-- и здесь мы переходим к этому:
-- третье вычисление - конечное, это применение окружения к новой построенной монаде:
-- \e -> k (m e) e
-- именно эта часть: \e, то есть принятие окружения как аргумент, и применение этого окружения к стреле Клейсли, позволяет
-- писать программу, неявно применяя окружение ко всем функциям

-- использование:
-- мы можем писать обычные функции без упаковывания их в контейнер, так как мы работает в окружении списка
-- и все функции будут неявно применяться с помощью оператора (>>=) к списку
safeHead = do
  b <- null {-здесь инструкция null, которая принимает какое-то значчение ( здесь окружение) и возвращает значение
  и есть на самом деле монада Reader, ведь действительно, типы полностью совпадают: (e -> a) -}
  if b
  then return Nothing {-упаковывать Nothing в монаду нужно потому, что мы работаем в монаде Reader-}
  else do
    h <- head
    return $ Just h {-снова упаковываем значение в монаду Reader-}

-- вариант с прямым использованием окружения
-- теперь в env кладется окружение, и мы конечно же можем использовать окружение так:
-- b <- null, так как оператор (<-) достает значение из контейнера, а null - как раз является контейнером типа (e -> a)
-- но мы не можем писать так: b <- null env, так как тип не совпадает, тип выражения null env такой: Bool
-- так как null env уже возвращает результат, типа Bool, и мы не можем применить окружение к такому контейнеру
-- однако, тепреь можем использовать так: if null env
-- очень важно следить за типами, и понимать, что достается из контейнера и в какой монаде мы работаем
safeHead' = do
  env <- id
  if null env
  then return Nothing {-упаковывать Nothing в монаду нужно потому, что мы работаем в монаде Reader-}
  else do
    return $ Just (head env) {-снова упаковываем значение в монаду Reader-}

-- еще один очень важный момент, как хаскель вообще понимает в какой Монаде мы работаем:
-- первая инструкция фиксирует нам тип первого параметра монады, то есть e
-- здесь тип e - это список, то есть тип [a], так как первая инструкия - (b <- null), а null требует списка
-- самая последняя инструкция в вычислении фиксирует нам тип возвращаемого значения
-- здесь: return $ Just h и return Nothing
-- при чем, если есть ветвление, в обоих ветвях возвращаемое значение должно быть одного и того же типа
-- здесь, возвращаемое значение - это тип Maybe a

-- мы можем убедиться в этом, спросив тип функции:
-- :t safeHead
--  -> safeHead :: [a] -> Maybe a
-- ну и также, на этом типе мы действительно убеждаемся, что в итоге мы получаем Монаду Reader, которую можно вычислить
-- и получить из неё значение, упакованное в неё
-- мы можем сделать это так:
-- safeHead [1,2,3]
--  -> Just 1
-- итак, сначала хаскель делает eval функции safeHead, в итоге строится Монада типа (e -> a)
-- потом применяется аргумент [1,2,3] к этой монаде, и мы получаем хранящееся внутри значение

-- оператор (>>) в цепочке вычислений в монаде Reader абсолютно бесполезен:
-- в то время как оператор (>>=) позволяет передать одно и то же значение (окружение) в качестве аргумента нескольким функциям
-- в цепочке композиций, так как он строит монаду типа (e -> b) в качестве возвращаемого значения, то к нему будет все время
-- применяться окружение
-- оператор (>>) игнорирует результат работы прошлого степа, и не может применять окружение к полученному с помощью стрелки
-- Клейсли монаде - \e -> k (m e) e
-- оператор (>>=):
-- оператор (>>): \_ -> k (m (что применять??)) (что применять??)

-- теперь напишем тип Reader и функции для более удобной работы с окружением и значением Монад
newtype Reader r a = Reader { runReader :: (r -> a) }

-- runReader :: Reader r a -> (r -> a)

instance Functor (Reader r) where

instance Applicative (Reader r) where

instance Monad (Reader r) where
  return x = Reader (\_ -> x) {-строим Монаду Reader, передавая в конструктор данных функцию (\_ -> x)-}
  m >>= k = Reader $ \e ->
    let v = runReader m e {-достаем функцию типа (r -> a) из переданной монады m
    с помощью функции достаем из Ридера значение v. Это вычисление раньше - (m e) -}
    in runReader (k v) e {-строим новую монаду типа Reader r a с помощью стрелки Клейсли,
    далее мы достаем функцию оттуда, и применяем к этой функции окружение e
    в итоге мы получили, как и раньше, новое вычисление
    runReader (k v) - достает функцию (e -> a)
    к этой функции применяется пераданное окружение e
    и мы получаем результат a, все как и раньше

    ВАЖНО, с помощью оператора (>>=) мы строим именно цепочку вычислений, поэтому запуск нового вычисления так важен
    возвращаем значение пользователю мы с помощью функции return
    то есть, сначала идет цепочка вычислений с помощью оператор (>>=) - в каждую инструкцию которой будет передаваться
    окружение, делаться новое вычисление, а потом мы просто вычисленное значение ложим в Монаду Reader и возвращаем
    упакованное значение
    пользователь сможет достать упакованное значение с помощью функции runReader, только теперь в конечном Ридере
    окружение будет игнорироваться, и просто вернется значение упакованное
    но ко всем предыдущим степам будет передаваться неявно окружение
   -}

-- мы видим, что все поведение точно то же самое, что и с частично примененной стрелкой, только теперь у нас есть функция
-- runReader для запуска вычисления. runReader применяется к монаде типа Reader r a, а возвращается функция (r -> a),
-- к этой функции мы можем применять окружение
-- раньше нам приходилось работать напрямую с функцией (e -> a), теперь за нас это делает функция runReader

-- напишем теперь функции для удобной работы с Ридерами:

-- функция ask просто спрашивает окружение и возвращает его Ридер, где внутри будет лежать функция id, тип которой полностью
-- удовлетворяет полю runReader - (e -> a), так как тип функции id - (a -> a)
-- в итоге, например такая инструцкия:
{-
env <- ask
-}
-- будет работать так:
-- ask строит новый ридер типа Reader r r, а как мы помним оператор <- достает значение из контейнера, то в переменной
-- evn теперь будет лежать окружение
ask :: Reader r r
ask = Reader id

-- теперь мы можем использовать окружение так:
-- env <- ask
-- не используя напрямую id (как в safeHead'), а работая на высоком уровне
-- использование:
-- runReader ask 42
--  -> 42

-- какая работа тут была проделана:
-- 1. функция ask построила Ридер, в котором лежит функция id
-- 2. Мы достали эту функцию из Ридера с помощью функции runReader
-- 3. Мы применили к функции Ридера окружение 42
-- мы получили результат - само окружение

-- почему функция названа вообще runReader?
-- на самом деле, можно думать по-другому, тогда станет понятнее
-- на самом деле тип функции runReader можно переписать так:
-- runReader :: Reader r a -> r -> a
-- теперь мы видим, что мы ЗАПУСКАЕМ ВЫЧИСЛЕНИЕ
-- передаем Ридер, передаем окружение, и получаем результат работы функции, которая хранится в этом Ридере
-- как мы выше сделали, runReader ask 42
-- мы запустили вычисление Ридера: передав туда как параметр сам Ридер, и окружение

-- пример использования ask:
type User = String
type Password = String
type UsersTable = [(User, Password)]

pwds :: UsersTable
pwds = [("Bill", "123"),("Ann", "qwerty"), ("John", "2sRq8P")]

-- тип возвращаемого значения - Reader, в качестве окружения которого выступает UsersTable, а возвращаемое значение -
-- User
-- значит все функции внутри монадического вычисления будут применяться к UsersTable
firstUser :: Reader UsersTable User
firstUser = do
  e <- ask {-достаем список юзеров - список пар-}
  return $ fst (head e) {-возвращаем имя первого пользователя-}

-- использование:
-- runReader firstUser pwds
--  -> "Bill"

-- перепишем в безопасном варианте - для пустых списков тоже
firstUser' :: Reader UsersTable (Maybe User)
firstUser' = do
  env <- ask {-ask неявно применяется к окружению и возвращает Монаду Reader, в котором упаковано окружение и окружение
  достается с помощью оператор (<-)-}
  case safeHead env of {-safeHead явно применяется к окружению - списку, и возвращется значение типа Maybe (User, Password)-}
    (Just (user, password)) -> return $ Just user
    Nothing                 -> return Nothing

-- ВАЖНО!!
-- case .. of - не цепочка вычислений, а обычное ветвление, как и if, это не относится к монадическим вычислениям!
-- поэтому функции в case .. of требуется писать как обычное вычисление, а не монадическое, в if тоже

-- использование:
-- runReader firstUser' pwds
--  -> Just "Bill"
-- runReader firstUser' []
--  -> Nothing

-- иногда окружение может быть довольно сложным типом и работать напрямую с ним не очень удобно
-- поэтому мы напишем функцию, которая выполнит над окружением какое-то действие и вернет нам какую-то часть окружения,
-- которая нам действительно требуется

-- такая функция - это функция asks
-- asks принимает какую-то функцию, которая выполняет над окружением действие, помещает ее в контейнер Reader, и с помощью
-- доставания значения из этой Монады мы получаем необходимую часть окружения
asks :: (r -> a) -> Reader r a
asks f = Reader f

firstUserPwd :: Reader UsersTable Password
firstUserPwd = do
  pwd <- asks (\xs -> snd $ head xs)
  return pwd

-- мы создаем Reader с функцией (\xs -> snd $ head xs), тип которой полностью
-- удовлетворяет типу функции в Reader:
-- :t (\xs -> snd $ head xs)
--  -> (\xs -> snd $ head xs) :: [(a, b)] -> b
-- здесь окружение e - это список пар [(a,b)], а возвращаемое значение - второй элементы пары, то есть тип b
-- далее к построенному Ридеру неявно передается окружение, и в переменную pwd соответственно записывается результат
-- вычисления этого Ридера, то есть пароль первого юзера

-- использование:
-- runReader firstUserPwd pwds
--  -> "123"

-- можно переписать функцию firstUserPwd проще:
-- так как мы сначала распаковывали Ридер, а потом снова упаковывали значение в Ридер, то от этого нет никакой пользы,
-- поэтому мы сразу возвращаем Ридер
firstUserPwd' = asks (\xs -> snd $ head xs)

-- напишем еще одну функцию для работы с нашим массивом:
-- usersCount создает Ридер, внутри которого лежит функция length, соответственно вычисление этого ридера с окружением
-- массивом вернет нам длину массива
usersCount :: Reader UsersTable Int
usersCount = asks length

-- использование:
-- runReader usersCount pwds
--  -> 3

-- теперь напишем функцию для работы с окружением
-- с помощью функции local мы сможем обобщить идеи фуфнкций firstUserPwd' и usersCount:
-- функция local локально модифицирует окружение, то есть обрабатывает текущее окружение и возвращает новое окружение
--
-- итак, что делает local - local строит новый Ридер, в котором находится функция,
-- которая принимает окружение (\e), модифицирует его (f e), а потом запускает вычисление на этом же ридере, но только
-- с новым окружением
-- тогда, так как в новом Ридере находится такая функция, то в возвращаемом ридере Reader r a будет сохранено новое значение
-- и запуск вычисления на этом ридере вернет новый результат, а не тот, который получился бы без изменения окружения

-- пример использования:
-- сравнение:
-- runReader usersCount pwds <- вычисление запусается на Ридере (usersCount) с окружением pwds
--  -> 3
-- и
-- runReader (local (("Mike","1"):) usersCount) pwds  <- вычисление запусается на Ридере (usersCount) с новым окружением
--  -> 4

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)

-- то есть, чтобы легче понять, можно переписать так:
-- Reader $ \e -> a <- здесь a - новое значение, это результат работы вычисления runReader m (f e), ну а такое вычисление
-- возвращает результат, так как тип функции Ридера - (e -> a)
-- итак, мы видим, что у нас получился новый ридер, в котором лежит функция, принимающая окружение и  возвращающая
-- новое значение после выполнения вычисления Ридера с новым окружением

-- пример использования функции local:
localTest :: Reader UsersTable (Int, Int)
localTest = do
  count1 <- usersCount
  count2 <- local (("Mike","1"):) usersCount
  return (count1, count2)

-- использование функции localTest:
-- runReader localTest pwds
--  -> (3,4)
-- runReader localTest []
--  -> (0,1)
