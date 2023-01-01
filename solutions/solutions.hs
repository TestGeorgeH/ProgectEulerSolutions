import Data.List
import Data.Ord

primes = sieve [2..]
    where   sieve (p:xs) = p : sieve ( filter (`notDivisibleBy` p) xs )
            sieve [] = []
            notDivisibleBy t p = t `rem` p /= 0

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

largestPrimeFactorOf n = divider n primesOpt
    where divider n (q:xs)
            | n == q = q
            | n `rem` q == 0 = divider (n `div` q) xs
            | otherwise = divider n xs
          
          divider n [] = n  
-----------------------------
-- 10001st prime
-- Problem 7

tenThousandAndFirstPrime = primesOpt !! 10001

-----------------------------
-- Summation of primes
-- Problem 10


primesUntil upperBound = takeWhile ( <= upperBound) primesOpt
primesSumUntil upperBound = sum $ primesUntil upperBound

-- testPrimes = (primesUntil 300) == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293]

-----------------------------
-- Circular primes
-- Problem 35


numDigits n = toInteger (floor (logBase 10 (fromIntegral n)) + 1)

rotateNumber n = numHead + numTail
    where numHead = (n `rem` 10) * 10 ^(numDigits n -1)
          numTail = (n `div` 10)

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

circularPrimesCandUntil n = filter canBeCand (primesUntil n)
    where canBeCand n = not (any (n `hasDigit`) [0,2,4,5,6,8])
          n `hasDigit` i = elem i $ map (`rem` 10) (rotationsOfNumber n) 

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

digits = map (read . (:[])) . show

fromDigits l = foldl1 (\x y -> 10*x+y) l

rmdups :: (Ord a) => [a] -> [a] -- they are just 4 digits, that will cut it
rmdups = map head . group . sort

-- replace by sets later

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] _    = True
isSubList _ []    = False
isSubList (x:xs) (y:ys) 
    | x == y    = isSubList xs ys   
    | otherwise = isSubList (x:xs) ys


permutateNumber n = (permutateNumber' n (numDigits n))
    where permutateNumber' n l = leaveNDigitNumbers l $ rmdups $ map (fromDigits) (permutations(digits n))

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
        

-- optinised to 20 sec run, good for now

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

tenDigitSelfPower n = helper n n
    where   helper n 0 = 1   
            helper n i = ((helper n (i-1)) * n) `rem` 10^(10 +1)

tenDigitSelfPowerSum n = sum [tenDigitSelfPower i | i <- [1..n]]

-------------------------
-- 1000-digit Fibonacci number
-- Problem 25

-- took this problem just because fib numbers are cute in Haskell

fib = 1 : next
    where next = 1 : zipWith (+) fib next

-- Explanation -- this is just a very fancy way of adding two numbers every time it's necessary
-- Cash is included, so it's linear 

nDigitFibNumber n = last $ zip [1..] $ takeWhile (\t -> (t `div` 10^n) == 0 ) fib
numberOfThousandDigitFibNumber = fst $ nDigitFibNumber 1000

-------------------------

-- Pentagon numbers
-- Problem 44

isSquare n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double) -- kinda ugly, but what to do

sqrt' n = floor $ sqrt $ (fromIntegral n::Double)

pentNumbers = [n*(3*n-1) `div` 2 | n <- [1..]]

isPentNumber n 
    | not $ isSquare (1+24*n) = False
    | otherwise = (sqrt' (1+24*n) `rem` 6) == 5

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

digitFactorials = [1,1,2,6,24,120,720,5040,40320,362880]

-- number of digits d  of number n lies in 10^(d-1) ≤ n ≤ 9!d, and it holds for d ≤ 7
-- Upper bound is 2_540_160, as largest sum of factorials is 9!*7
-- (it can be heavily reduced, but it requires elaborate explanations)

sumFactDigits n = sum $ map ((!!) digitFactorials) (digits n)

specialFactNumbers = [i|  i <- [0..2540160], sumFactDigits i == i]

------------------------

-- Longest Collatz sequence
-- Problem 14

collatz n
    | n == 1 = [n]
    | n `rem` 2 == 0 = n : collatz (n `div` 2)
    | otherwise = n : collatz (3*n +1)

longestCollatzUnder n = maximumBy (comparing length) [collatz i | i <- [1..100]]

-- main :: IO()
-- main = putStrLn (show (longestCollatzUnder 1_000_000))

main :: IO()
main = putStrLn (show (specialFactNumbers))