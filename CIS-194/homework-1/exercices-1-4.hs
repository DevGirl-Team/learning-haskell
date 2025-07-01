-- Exercise 1

mod10 :: Integer -> Integer
mod10 x = x `mod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = (mod10 x):toDigitsRev(divBy10 x)
    where
        divBy10 x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- Exercise 1 bonus

toDigitsWthtRev :: [Integer] -> Integer -> [Integer]
toDigitsWthtRev result num
    | num <= 0 = []
    | divBy10 num == 0 = num:result
    | otherwise = toDigitsWthtRev ((mod10 num):result) (divBy10 num)
    where
        divBy10 x = x `div` 10

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [ doubleIfEvenIndex i n | (i, n) <- xsWithIndexes ]
    where 
        xsRev = reverse xs
        xsWithIndexes = zip [0..] xsRev
        doubleIfEvenIndex i n = if i `mod` 2 /= 0 then n * 2 else n



-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum (toDigits n) | n <- xs]

-- Exercise 4

validate :: Integer -> Bool
validate cardNumber = mod10 (sumDigits (doubleEveryOther (toDigits cardNumber))) == 0