-- Exercise 1 Hopscotch

-- map a list from the length of the input list starting from 2, as the first entry is the input list itself
-- in the map function, the input list zipped as tuples to filter on the indexes
-- the tuple list is filtered to keep all the indexes that are multiple of the current element of the mapped list
-- map the tuple list to keep the first element
skips :: [a] -> [[a]]
skips [] = []
skips l = l:map (\x -> map fst . filter (\(_,i) -> i `mod` x == 0) $ zip l [1..]) [2..(length l)]

-- Exercise 2 Local maxima

localMaxima l = map fst . filter (\(x,i) -> (i>0 && i<length l - 1) && x > (l!!(i-1)) && x > (l!!(i+1))) $ zip l [0..]