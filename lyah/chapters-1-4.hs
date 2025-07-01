doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

boomBangs xs = [ if x > 10
                    then "BANG!"
                    else "BOOM!"
                | x <- xs, odd x ]

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

square x = x * x

keepUppercaseLetters :: String -> String
keepUppercaseLetters str = [ c | c <- str, c `elem` ['A'..'Z'] ]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x - 1)

sumTuples :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "The list can't be empty"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "The list can't be empty"
tail' (_:xs) = xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "1 elem " ++ show x
tell (x:y:[]) = "2 elems " ++ show x ++ " and " ++ show y
tell (x:y:_) = "the list is long " ++ show x ++ " and " ++ show y

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y = x
    | otherwise = y

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a < b = LT
    | a == b = EQ
    | otherwise = GT

initiales :: String -> String -> String
initiales firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname