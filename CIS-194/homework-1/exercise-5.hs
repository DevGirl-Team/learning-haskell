-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end tmp
    | n == 1 = [(start, end)]
    | otherwise = (hanoi (n-1) start tmp end) ++ [(start, end)] ++ (hanoi (n-1) tmp end start)

-- Exercise 6

