{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Ord
import Data.FastDigits (digits, undigits)
import Math.NumberTheory.Roots (integerSquareRoot, isSquare)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Map.Internal.Debug (showTreeWith)

-- TODO:
--     add normal tests 
--     add signatures for functions

primes :: (Integral a) => [a]
primes = sieve [2..]
    where   sieve (p:xs) = p : sieve ( filter (`notDivisibleBy` p) xs )
            sieve [] = []
            notDivisibleBy t p = t `rem` p /= 0

primesOpt :: (Integral a) => [a]
primesOpt = filter isPrime [2..]
isPrime n = check n 2
    where
        check n d
            | d*d > n        = True
            | n `rem` d == 0 = False
            | otherwise      = check n (d+1)

-----------------------------
-- Largest prime factor
-- Problem 3
-- 600851475143

largestPrimeFactorOf :: (Integral a) => a -> a
largestPrimeFactorOf n = divider n primesOpt
    where divider n (q:xs)
            | n == q = q
            | n `rem` q == 0 = divider (n `div` q) xs
            | otherwise = divider n xs
          
          divider n [] = n  
-----------------------------
-- 10001st prime
-- Problem 7

tenThousandAndFirstPrime :: (Integral a) => a
tenThousandAndFirstPrime = primesOpt !! 10001

-----------------------------
-- Summation of primes
-- Problem 10

primesUntil :: (Integral a) => a -> [a]
primesUntil upperBound = takeWhile ( <= upperBound) primesOpt

primesSumUntil :: (Integral a) => a -> a
primesSumUntil upperBound = sum $ primesUntil upperBound

-----------------------------
-- Circular primes
-- Problem 35

numDigits :: (Integral a) => a -> a
numDigits n = floor (logBase 10 (fromIntegral n)) + 1

rotateNumber :: (Integral a) => a -> a
rotateNumber n = numHead + numTail
    where numHead = (n `rem` 10) * 10 ^(numDigits n -1)
          numTail = (n `div` 10)

rotationsOfNumber :: (Integral a) => a -> [a]
rotationsOfNumber n = rotationsHelper n (numDigits n)
    where rotationsHelper n 0 = []
          rotationsHelper n i = n : (rotationsHelper (rotateNumber n) (i-1))

-- A circular prime with at least two digits 
--     can only consist of combinations of the digits
--     1, 3, 7 or 9, because having 0, 2, 4, 6 or 8 as the last digit 
--     makes the number divisible by 2, 
--     and having 0 or 5 as the last digit
--     makes it divisible by 5.
--     Wikipedia

circularPrimesCandUntil :: (Integral a) => a -> [a]
circularPrimesCandUntil n = filter canBeCand (primesUntil n)
    where   
            canBeCand :: (Integral a) => a -> Bool
            canBeCand n = not (any (n `hasDigit`) [0,2,4,5,6,8])
            hasDigit :: (Integral a) => a -> Int -> Bool
            n `hasDigit` i = elem i (digits 10 (fromIntegral n :: Integer))

circularPrimesUntil :: (Integral a) => a -> [a]
circularPrimesUntil n = concat $ circularPrimesUntil' 2 (circularPrimesCandUntil n)
    where   rot c = rotationsOfNumber c
            calculateRest c candidates = (circularPrimesUntil' (head (candidates \\ (rot c))) (candidates \\ (rot c)))

            circularPrimesUntil' _ [] = []
            circularPrimesUntil' c candidates
                | all (\t -> elem t candidates) (rot c) = (rot c) : (calculateRest c candidates)
                | otherwise = (calculateRest c candidates)

-----------------------------
-- Prime permutations
-- Problem 49

-- the solution is slow and ugly

leaveNDigitNumbers n = filter (\t -> numDigits t == n)

fourDigitPrimes :: (Integral a) => [a]
fourDigitPrimes = leaveNDigitNumbers 4 (primesUntil 10000)

rmdups :: (Ord a) => [a] -> [a] -- they are just 4 digits, that will cut it
rmdups = map head . group . sort

mapExample = helper [1,2,3,4,5]
    where 
            helper [] = Map.empty
            helper (x:xs) = Map.insert x x (helper xs)  

genKey :: (Integral a) => a -> a
genKey n = fromIntegral $ undigits 10 $ sort $ digits 10 (fromIntegral n)

keyedFourDigitPrimes :: (Integral a) => Map a [a]
keyedFourDigitPrimes = mapper fourDigitPrimes
    where   mapper :: (Integral a) => [a] -> Map a [a]
            mapper [] = Map.empty
            mapper (n:xs)
                | isPrime n = Map.insertWith (++) (genKey n) [n] (mapper xs)
                | otherwise = mapper xs 

prettyFourDigitPrimesCandidates :: (Integral a) => [[a]]
prettyFourDigitPrimesCandidates = map (sort.snd) $ Map.toList keyedFourDigitPrimes

-- can be done faster, but sequences are no longer then 10

isArithmSeq :: (Num a, Eq a) => [a] -> Bool
isArithmSeq [] = False
isArithmSeq [x] = False
isArithmSeq [x,y] = True
isArithmSeq (x:y:z:xs) = (x - y) == (y - z) && isArithmSeq (y:z:xs)

arithSubseq :: (Integral a) => [a] -> [[a]]
arithSubseq seq = filter isArithmSeq $ subsequences seq
maxArithSubseq :: (Integral a) => [a] -> [a]
maxArithSubseq [] = []
maxArithSubseq [x] = [x]
maxArithSubseq seq = maximumBy (comparing length) $ arithSubseq seq

-- optimised to a couple of seconds
largestPrettyFourDigitPrimes = filter (\t -> length t == maxLen) candidates
    where   
            candidates :: (Integral a) => [[a]]
            candidates = map maxArithSubseq  prettyFourDigitPrimesCandidates
            maxLen :: (Integral a) => a
            maxLen = fromIntegral $ maximum $ map length candidates

-------------------------
-- primes are boring, I'll go do something else

-- Self powers
-- Problem 48

tenDigitSelfPower :: (Integral a) => a -> a
tenDigitSelfPower n = helper n n
    where   helper n 0 = 1   
            helper n i = ((helper n (i-1)) * n) `rem` 10^(10 +1)

tenDigitSelfPowerSum :: (Integral a) => a -> a
tenDigitSelfPowerSum n = sum [tenDigitSelfPower i | i <- [1..n]]

-------------------------
-- 1000-digit Fibonacci number
-- Problem 25

-- took this problem just because fib numbers are cute in Haskell

fib :: (Integral a) => [a]
fib = 1 : next
    where next = 1 : zipWith (+) fib next

-- Explanation -- this is just a very fancy way of adding two numbers every time it's necessary
-- Cash is included, so it's linear 

-- returns # and fib number itself
nDigitFibNumber :: (Integral a) => a -> (a, a)
nDigitFibNumber n = last $ zip [1..] $ takeWhile (\t -> (t `div` 10^n) == 0 ) fib

numberOfThousandDigitFibNumber :: (Integral a) => a
numberOfThousandDigitFibNumber = fst $ nDigitFibNumber 1000

-------------------------

-- Pentagon numbers
-- Problem 44

pentNumbers = [n*(3*n-1) `div` 2 | n <- [1..]]

isPentNumber n 
    | not $ isSquare (1+24*n) = False
    | otherwise = (integerSquareRoot (1+24*n) `rem` 6) == 5

cutePentNumbers = helper 2
    where helper n = 
            let pnums = take n pentNumbers
                pn = last pnums
                candidates = [sort [pn - i, i] | i <- init pnums]
                filtered = filter (\t -> isPentNumber(last t) && isPentNumber(last t - head t)) candidates
            in (filtered) ++ (helper (n+1))

smallestCuteNumber = head $ map (\t -> (last t) - (head t)) cutePentNumbers

------------------------

-- Digit factorials
-- Problem 34
digitFactorials :: (Integral a) => [a]
digitFactorials = [1,1,2,6,24,120,720,5040,40320,362880]

-- number of digits d  of number n lies in 10^(d-1) ≤ n ≤ 9!d, and it holds for d ≤ 7
-- Upper bound is 2_540_160, as largest sum of factorials is 9!*7
-- (it can be heavily reduced, but it requires elaborate explanations)

sumFactDigits :: (Integral a) => a -> a
sumFactDigits n = sum $ map ((!!) digitFactorials) (digits 10 (fromIntegral n))

specialFactNumbers :: (Integral a) => [a]
specialFactNumbers = [i | i <- [0..2540160], sumFactDigits i == i]

-- Longest Collatz sequence
-- Problem 14

collatz :: (Integral a) => a -> [a]
collatz n
    | n == 1 = [n]
    | n `rem` 2 == 0 = n : collatz (n `div` 2)
    | otherwise = n : collatz (3*n +1)

longestCollatzUnder :: (Integral a) => a -> [a]
longestCollatzUnder n = maximumBy (comparing length) [collatz i | i <- [1..n]]

main :: IO()
main = putStrLn (show (largestPrettyFourDigitPrimes))

-- main :: IO()
-- main = putStrLn $ showTreeWith (\k x -> show (k,x)) True False keyedFourDigitPrimes