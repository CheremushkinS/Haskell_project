binadd :: [Int] -> [Int] -> Int -> [Int]
binadd [] n a = if a /=0 then a:n else n -- !!
--binadd n [] a = if a /=0 then a:n else n
binadd x n a = (nx `mod` 2):(binadd (tail x) (tail n) (nx `div` 2))
             where 
             nx = (head x) + (head n) + a
             
binadd' :: [Int] -> [Int] -> [Int]                      
binadd' x y | length xx > length yy = reverse $ binadd xx (yy ++ (take (length xx - length yy) [0, 0..])) 0
            | otherwise = reverse  $ binadd (xx ++ (take (length yy - length xx) [0, 0..]) ) yy 0
 where
 xx = reverse x
 yy = reverse y


--if length xx > length yy then yy = (take (length xx - length yy) [0, 0..]) ++ y else xx = (take (length yy - length xx) [0, 0..]) ++ xx
 
 
 