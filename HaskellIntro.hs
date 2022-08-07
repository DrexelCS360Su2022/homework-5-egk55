{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set ( Set )

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = if n >= 10 then lastDigit (rem n 10) else n

dropLastDigit :: Integer -> Integer
dropLastDigit n = if n < 10 then 0 else div n 10

toDigits :: Integer -> [Integer]
toDigits n = if n <= 0 then [] else lastDigit n : toDigits(dropLastDigit n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = map (\temp -> if (rem temp 2) == 1 then (2 * temp) else temp) list

sumDigits :: [Integer] -> Integer
sumDigits list = if list == [] 
    then 0 
    else (if (head list) >= 10 
        then ((lastDigit (head list)) + dropLastDigit(head list)) 
        else (head list)) + sumDigits (tail list)

validate :: Integer -> Bool
validate n = if (rem (sumDigits (doubleEveryOther (toDigits n))) 10) == 0
    then True else False

--
-- Problem 2
--

pow :: (a -> a) -> Integer -> a -> a
pow f n = if (n <= 1) 
    then f
    else pow (f . f) (n - 1)



g :: Integer -> Integer
g n = if (n <= 0)
    then 0
    else n - (pow g(n - 1) 2)

h :: Integer-> Integer
h n = if (n <= 0)
    then 0
    else n - (pow h(n - 1) 3)

d :: Integer -> Integer -> Integer
d i n = if (n <= 0)
    then 0
    else n - (pow d(i n-1) i)
--
-- Problem 3
--

powerSet :: Set a -> Set temp -> Set a
powerSet s = if (isEmpty s )
    then Set []
    else 
        split(s) = (x, xs)
        (x : powerSet xs) ++ powerSet xs
