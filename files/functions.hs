tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has " ++ show x ++ show y ++ " as the first 2 elems"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "underweight"
    | weight / height ^ 2 <= 25.0 = "normal"
    | weight / height ^ 2 <= 30.0 = "overweight"
    | otherwise = "certified whale"

max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' height weight
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "overweight"
    | otherwise = "certified whale"
    where bmi = weight / height ^ 2
