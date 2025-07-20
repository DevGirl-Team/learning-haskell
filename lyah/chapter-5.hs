maximum' :: (Ord a) => [a] -> a
maximum' [] = error "The list can't be empty"
maximum' (x:[]) = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n t
    | n <= 0 = []
    | otherwise = t:(replicate' (n-1) t)

take' :: Int -> [a] -> [a]
take' n l
    | n <= 0 = []
    | otherwise = case l of [] -> []
                            (x:xs) -> x:(take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' t = t:(repeat' t)

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)
zip' _ _ = []

elem' :: (Eq a) => a -> [a] -> Bool
elem' t (x:xs) = if t == x then True else elem' t xs
elem' _ _ = False

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort [s | s <- xs, s <= x]) ++ [x] ++ (quickSort [b | b <- xs, b > x])