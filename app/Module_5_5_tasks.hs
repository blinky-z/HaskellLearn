module Module_5_5_tasks where

import           Data.List        (isInfixOf)
import           System.Directory

--
-- https://stepik.org/lesson/8443/step/3?unit=1578
main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if name /= ""
    then putStrLn ("Hi, " ++ name ++ "!")
    else main'

--
-- https://stepik.org/lesson/8443/step/8?unit=1578
main'' :: IO ()
main'' = do
  putStr "Substring: "
  substr <- getLine
  if substr == "" then putStrLn "Canceled" else
    do
      xs <- getDirectoryContents "."
      mapM_ (\x -> if substr `isInfixOf` x then do removeFile x; putStrLn ("Removing file: " ++ x) else putStr "") xs

-- другое решение
main1'' :: IO ()
main1'' = do
  putStr "Substring: "
  substr <- getLine
  if substr == "" then putStrLn "Canceled" else
    do
      xs <- getDirectoryContents "."
      filtered <- return $ filter (isInfixOf substr) xs
      mapM_ (\x -> do removeFile x; putStrLn ("Removing file: " ++ x)) filtered
