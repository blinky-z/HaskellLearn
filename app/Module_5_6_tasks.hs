module Module_5_6_tasks where

import Module_5_6

--
-- https://stepik.org/lesson/8441/step/3?unit=1576
-- Не используя интерпретатор, вычислите значение следующего выражения:
--
-- return 2 >>= (+) >>= (*) $ 4

a = do
  x <- return 2 {-по определению return функция return принимает окружение и значение, игнорирует окружение и
   возвращает значение-} {-return x = \_ -> x-}
  y <- (+) x {-снова монадическое вычисление - наша Монада имеет тип (e -> a), то есть принимает какое-то окружение и
  возвращает значение-}
  (*) y {-снова монадическое вычисление, тип Монады - (e -> a), значит снова передается окружение-}

-- тогда вызов a 4 равен
--  -> 24


--
-- https://stepik.org/lesson/8441/step/7?unit=1576
local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)

--
-- https://stepik.org/lesson/8441/step/9?unit=1576
usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  env <- ask
  filtered <- return $ filter (\(_, pwd) -> pwd == "123456") env
  return $ map (\(user, _) -> user) filtered

-- упаковка нового массива нужна потому, что мы работаем в монаде Reader, а поэтому сначала
-- мы должны построить Монаду, в которой будет храниться значение, потом в нее неявно передастся окружение, но оно будет
-- проигнорировано, так как return строит Монаду, в которой окружение игнорируется и просто возвратится значение
-- и соотвественно значение сохранится в переменную filtered
-- вообще, когда мы передаем состояние в Монаду Reader, то оно автоматически передается вссм функциям, содержащимся в этой
-- монаде (в цепочке вычислений)

-- возвращаем тоже монаду, так как именно на ней будет запущено вычисление, и пользователь получит результат работы, передав
-- окружение