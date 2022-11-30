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
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= overweight = "overweight"
    | otherwise <= "certified whale"
