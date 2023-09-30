data Tree a =  Empty | Branch a(Tree a)(Tree a) deriving Show

addword :: Tree String -> String -> Tree String
addword Empty x = Branch x Empty Empty
addword (Branch v l r) x | x > v = (Branch v l (addword r x))
                         | otherwise = (Branch v (addword l x) r)

addstr :: String -> Tree String 
addstr str = foldr (\ w acc -> addword acc w) Empty (words str)

wordsearch :: Tree String -> String -> Bool
wordsearch Empty _ = False
wordsearch (Branch v l r) w | v == w = True
                            | w < v = wordsearch l w
                            | otherwise = wordsearch r w    

nodecount :: Tree a -> Integer
nodecount Empty = 0
nodecount (Branch v l r) = (nodecount l) + (nodecount r) + 1

t2labc :: Tree a -> [a] 
t2labc Empty = []
t2labc (Branch v l r) = (t2labc l) ++ [v] ++ (t2labc r)            

t2lcba :: Tree a -> [a] 
t2lcba Empty = []
t2lcba (Branch v l r) = (t2lcba r) ++ [v] ++ (t2lcba l)

nc' :: Tree a -> Int
nc' (Branch v l r) = length $ t2labc (Branch v l r)            

sum'' :: [Int] -> [Int]
sum'' [] = []
sum'' x = (sum x):(sum'' (tail x))

z :: [Int] -> [Int]
z x = map (\ n -> if n `mod` 2 == 0 then n + 1 else n * 2 ) x

intsearch :: [Integer] -> Integer -> Bool
intsearch [] _ = False
intsearch (x:xs) n | n == x = True
                   | otherwise = intsearch xs n  
                   
poli :: [Int] -> Int -> Int
poli l x = foldl (\ acc  n -> acc * x + n) 0 l

