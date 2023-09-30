
rep n x = take n $ (repeat x)

mkScr n m = rep n $ (rep m ' ')

setAst :: [String] -> Int -> Int -> [String]
setAst scr n m | (n >= length scr) || (m >= (length (head scr))) = scr
               | otherwise = l ++ [v] ++ r
                 where l=take n scr  
                       r=drop (n+1) scr
                       u=scr !! n
                       lv=take m u
                       rv=drop (m+1) u
                       v=lv ++ "*" ++ rv				 

norm :: Int -> [Double] -> [Int]
norm n ys = map (\ y -> truncate $ (y-ymin)/dy) ys
          where ymax = (maximum ys)
                ymin = (minimum ys)
                dy   = (ymax-ymin)/(fromIntegral n)

draw :: [String] -> [Int] -> Int -> [String]
draw scr [] _ = scr
draw scr (y:ys) m = draw (setAst scr y m) ys (m+1)

--graph n m = draw (mkScr n m) (norm n (map (\ x -> (sin (2*x)) * exp (-0.2*x)) [0,0.1..7.9])) 0
graph n m = draw (mkScr n m) (norm n (map (\ x -> (x-4)**3) [0,0.1..7.9])) 0

main = do 
       mapM putStrLn $ graph 25 80
       return ()
       
