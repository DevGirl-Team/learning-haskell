multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

isUppercase :: Char -> Bool
isUppercase = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x1:l1) (x2:l2) = (f x1 x2):(zipWith' f l1 l2)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:l) = f x : map' f l

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:l) = if f x then x : filter' f l else filter' f l

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort (filter (<=x) xs)) ++ [x] ++ (quickSort (filter (>x) xs))

compo :: (a -> b) -> (b -> c) -> a -> c
compo f1 f2 x = f2 (f1 x)

largestDivisible :: (Integral a) => a -> a
largestDivisible x = head (filter f [100000,99999..])
    where f = compo (`mod` x) (== 0)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else  takeWhile' f []

oddSquares :: (Integral a) => a -> [a]
oddSquares x = takeWhile (<x) (map (^2) (filter odd [1..]))

collatzSuite :: (Integral a) => a -> [a]
collatzSuite x
    | x <= 0 = []
    | x == 1 = [1]
    | even x = x : collatzSuite (x `div` 2)
    | otherwise = x : collatzSuite ((x * 3) + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatzSuite [1..100]))
    where isLong xs = length xs > 15

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' e l = foldl (\acc x -> acc || e == x) False l

map'' :: (a -> b) -> [a] -> [b]
map'' f l = foldr (\x acc -> (f x):acc) [] l

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (\x acc -> x * acc)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f l = foldr (\x acc -> if f x then x:acc else acc) [] l

head' :: [a] -> a
head' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

sqrtSum :: Int
sqrtSum = length (takeWhile (<1000) (scanl1 (\x acc -> x + acc) (map sqrt [1..]))) +1

negateAll :: (Num a) => [a] -> [a]
negateAll = map (negate . abs)

oddSquareSum :: Int
oddSquareSum = sum . takeWhile (< 10000) . map (^2) . filter odd $ [1..]