module Lib
  ( someFunc
  ) where

import Control.Monad
import Data.List
import Data.Char
import Data.Function

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- First class functions
-- no difference between var and func definition
hello = greet "World"

greet name = "Hello, " ++ name ++ "!"

-- Infix functions
-- every binary function can be used as an infix
-- every operator is actually a function!
prefixTest = mod 9 3

infixTest = 9 `mod` 3

addPrefix = (+) 1 2

-- Guards
-- no need for if else blocks
-- guess what otherwise actually means ;-)
fizzBuzz n
  | n `mod` 15 == 0 = "Fizz-Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

-- Function composition
-- prevent nested brackets
upperCaseFirst sentence = toUpper (head sentence) : tail sentence
upperCaseFirst3 sentence = (toUpper . head) sentence : sentence

filterPhone phone = (map digitToInt . filter isDigit) phone

-- Pattern matching
fac 0 = 1
fac n = n * fac (n - 1)

secondFromTuple (_, second) = second

secondFromTupleAsCase tuple =
  case tuple of
    (_, second) -> second

upperCaseFirstWithPatternMatching [] = []
upperCaseFirstWithPatternMatching (x:xs) = toUpper x : xs

-- Partial application
-- all functions are automatically curried
-- useful when parameters have a different lifetime
divisibleBy n d = n `mod` d == 0

divisibleBy3 = flip divisibleBy 3

fizzBuzzRules = [(flip divisibleBy 3, "Fizz"), (flip divisibleBy 5, "Buzz")]

fizzBuzzWithRules n
  | null result = show n
  | otherwise = result
  where
    result = (intercalate "-" . map snd . filter (\(appliesTo, _) -> appliesTo n)) fizzBuzzRules

-- Currying
-- you can (un-)curry all functions
-- function parameters in other functions are actually a parameter tuple!
curried = curry fst 1 2

uncurried = uncurry (+) (1, 2)

divOnTuple :: (Int, Int) -> Bool
divOnTuple (n, d) = uncurry divisibleBy (n, d)

-- List comprehension
-- combined filter and map on a list
fizzBuzzListComprehension n = joinResult [result | (appliesTo, result) <- fizzBuzzRules, appliesTo n]
  where
    joinResult [] = show n
    joinResult result = intercalate "-" result

quicksort [] = []
quicksort all@(x:xs) = quicksort left ++ mid ++ quicksort right
  where
    left = [y | y <- xs, y < x]
    right = [y | y <- xs, y > x]
    mid = [y | y <- all, y == x]

-- Folding/Unfolding
-- alternative to recursion
sumList :: [Int] -> Int
sumList list = foldr calc 0 list
  where
    calc element acc = element + acc

countDown :: Int -> [Int]
countDown n = unfoldr make n
  where
    make 0 = Nothing
    make n = Just (n, n - 1)

-- Eta Reduction
-- replace a lambda by the function itself,
-- x -> fun c => fun
-- lowerStr str = map toLower str
lowerStr :: String -> String
lowerStr = map toLower

-- WTF
-- Lifting
isOrdered :: String -> Bool
-- isOrdered a = sort a == a
-- isOrdered a = sort a == id a
-- isOrdered a = (liftM2 (==) sort id) a
isOrdered = return (==) `ap` sort `ap` id

upperCaseFirstWtf :: String -> String
-- upperCaseFirstWtf = liftM2 (:) (toUpper . head) tail
upperCaseFirstWtf = return (:) `ap` (toUpper . head) `ap` tail

-- Function application with on
-- equalsIgnoreCase a b = map toLower a == map toLower b
equalsIgnoreCase = (==) `on` map toLower

-- Crazy stuff with >>=
areAllUnique :: String -> Bool
-- areAllUnique a = nub a == a
areAllUnique = nub >>= (==)

-- mirror a = a ++ (reverse . init) a
-- mirror a = flip (++) ((reverse . init) a) a
mirror = reverse . init >>= flip (++)

isOrderedBind = sort >>= (==)
