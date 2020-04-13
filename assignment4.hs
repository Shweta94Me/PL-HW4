--Question 1:
maxlist :: (Ord a) => [a] -> a
maxlist [] = error "empty list"
maxlist [x] = x
maxlist (x:xs) = max x (maxlist xs)

--Question 2:
deletekth :: Int -> Int -> [a] -> [a]
deletekth k n [] = []
deletekth k n (x:xs)
    | n == 1 = deletekth k k xs
    | n > 1 = x : deletekth k (n-1) xs

delete :: Int -> [a] -> [a]
delete _ [] = []
delete 1 (_:_) = []
delete k lt = deletekth k k lt


--Question 3:
sort :: (Ord a) => a -> [a] -> [a]
sort x [] = [x]
sort x (y:ys)
    | x > y = y : sort x ys
    |otherwise = x : y : ys

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = sort x (isort xs)

--Question 4:
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate _ [x] = [x]
rotate n lt = drop (length lt - n) lt ++ take (length lt - n) lt

--Question 5:
signleton :: [a] -> [[a]]
signleton [] = []
signleton (x:xs) = [x] : signleton xs

single :: [a] -> [[a]]
single [] = [[]]
single [x] = [[x]]
single lt = signleton lt


--Question 6:
double :: [Int] -> [Int]
double [] = []
double [x] = [x*2]
double (x:y:ys) = x*2 : y : double ys







