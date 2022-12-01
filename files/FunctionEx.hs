-- checks if an element is in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = (e == x) || elem' e xs


-- removes all duplicates from a list
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs)
    | elem' x xs = nub' xs
    | otherwise = x : nub' xs


-- true if list is in ascending order
isAsc' :: [Int] -> Bool
isAsc' [] = True
isAsc' [a] = True
isAsc' (x:y:xs)
    | x <= y = True
    | x > y = False
    | otherwise = isAsc' (y:xs)


-- determines if a path from one node to another exiss within a directed graph
hasPath' :: [(Int, Int)] -> Int -> Int -> Bool
hasPath' [] x y = x == y
hasPath' xs x y
    | x == y = True
    | otherwise = 
        let xs' = [ (n, m) | (n, m) <- xs, n /= x ] in
        or [ hasPath' xs' m y | (n, m) <- xs, n == x ]
