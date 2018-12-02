{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LibHaskellBool
  ( prod
  , checkLocalHost
  , analyzeGold
  , analyzeGold2
  , analyzeGold3
  , addTo
  , makeAlias
  , clientId
  ) where

square :: Int -> Int
square v = v * v

prod :: Int -> Int -> Int
prod x y = x * y

checkLocalHost :: String -> String
checkLocalHost ip =
  if ip == "127.0.0.1" || ip == "0.0.0.0"
    then "It's a localhost!"
    else "No, it's not a localhost"

checkLocalHostResult = putStrLn (checkLocalHost "127.0.0.2")

analyzeGold :: Int -> String
analyzeGold standard =
  if | standard == 999 -> "Wow! 999"
     | standard == 750 -> "750"
     | standard == 550 -> "585"
     | otherwise -> "I don't know such a standard..."

analyzeGold2 :: Int -> String
analyzeGold2 standard
  | standard == 999 = "Wow! 999"
  | standard == 750 = "750"
  | standard == 550 = "585"
  | otherwise = "I don't know such a standard..."

analyzeGold3 :: Int -> String
analyzeGold3 999 = "Wow! 999"
analyzeGold3 750 = "750"
analyzeGold3 550 = "585"
analyzeGold3 _   = "I don't know such a standard..."

analyzeGold4 :: Int -> String
analyzeGold4 standard =
  case standard of
    999 -> "Wow! 999"
    750 -> "750"
    550 -> "585"
    _   -> "I don't know such a standard..."

calculateTime :: Int -> Int
calculateTime timeIns =
  if | timeIns < threshold -> timeIns + correction
     | timeIns >= threshold ->
       let delta = 1 + 2
        in timeIns + correction + delta
  where
    threshold = 40
    correction = 120

addTo :: String -> [String] -> [String]
addTo newHost hosts = newHost : hosts

makeAlias :: String -> String -> (String, String)
makeAlias host alias = (host, alias)

type UUID = String

type Name = String

type Email = String

type Age = Int

type Client = (UUID, Name, Email, Age)

clientId :: Client -> Email
clientId (_, _, email, _) = email

isNumOddPositive :: Int -> Bool
isNumOddPositive num =
  isOdd num && isPositive num
    where {-local funcs-}
     isOdd num = num `mod` 2 == 1
     isPositive num = num > 0

