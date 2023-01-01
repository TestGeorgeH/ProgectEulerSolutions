import Data.List
import Data.Ord
import Data.FastDigits (digits, undigits)
import Math.NumberTheory.Roots (integerSquareRoot, isSquare)

-- TODO:
--     add sets
--     add normal tests 
--     add signatures for functions

primes :: (Integral a) => [a]
primes = sieve [2..]
    where   sieve (p:xs) = p : sieve ( filter (`notDivisibleBy` p) xs )
            sieve [] = []
            notDivisibleBy t p = t `rem` p /= 0

primesOpt :: (Integral a) => [a]
primesOpt = filter isPrime [2..]
    where   isPrime n = check n 2
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

-- testPrimes = (primesUntil 300) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293]

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

fourDigitPrimes = leaveNDigitNumbers 4 (primesUntil 10000)

rmdups :: (Ord a) => [a] -> [a] -- they are just 4 digits, that will cut it
rmdups = map head . group . sort

-- TODO replace to sets later

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] _    = True
isSubList _ []    = False
isSubList (x:xs) (y:ys) 
    | x == y    = isSubList xs ys   
    | otherwise = isSubList (x:xs) ys


permutateNumber n = (permutateNumber' n (numDigits n))
    where permutateNumber' n l = leaveNDigitNumbers l $ rmdups $ map (undigits 10) (permutations(digits 10 n))

-- is more elegant way avaliable? (without sort and filter)
permutateNumberByThree n = rmdups $ map sort ([[x,y,z] | x <- perms n, y <- (perms n) \\ [x], z <- (perms n) \\ [x, y]])
    where perms n = permutateNumber n 

permutatedPrimes = permutatedPrimes' (head fourDigitPrimes) fourDigitPrimes
    where   perm c = permutateNumberByThree c

            calculateRest c candidates = (permutatedPrimes' (head (candidates \\ concat (perm c))) (candidates \\ concat (perm c)))

            permutatedPrimes' _ [] = []
            permutatedPrimes' c candidates 
                | any (\t -> t `isSubList` candidates) (perm c) = ((filter (\t -> t `isSubList` candidates) (perm c))) ++ (calculateRest c candidates)
                | otherwise = (calculateRest c candidates)
        

-- optimised to 20 sec run, good for now

permutatedPrimesWithEqDiffs = filter areDiffsEqual permutatedPrimes 
    where   areDiffsEqual (a:b:c:xs) = nc-nb == nb-na
                where s = sort([a,b,c])
                      nc = s!!2
                      nb = s!!1
                      na = s!!0
            areDiffsEqual _ = False

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

-- main :: IO()
-- main = putStrLn (show (longestCollatzUnder 1000000))

main :: IO()
main = putStrLn (show (permutatedPrimesWithEqDiffs))