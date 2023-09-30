
import System.Environment
import System.IO

-- Парсер

parse :: String -> String -> [String]
parse "" acc = if null acc then [] else [acc]
parse (x:xs) acc | x `elem` "()+-*/^x" = if null acc then 
                                           [[x]]++(parse xs "") 
                                         else 
                                           [acc]++[[x]]++(parse xs "")
                 | otherwise = parse xs (acc++[x])

-- Приоритеты операций
                 
prty :: String -> Int
prty "(" = 0
prty "+" = 1
prty "-" = 1
prty "*" = 2
prty "/" = 2
prty "^" = 3

-- Опустошить стек

empty :: [String] -> [Double] -> Double
empty [] n = head n
empty (o:os) (n2:n1:ns) = empty os (n:ns)
                          where n = exec o n1 n2

-- Выполнить бинарную операцию                          
                          
exec :: String -> Double -> Double -> Double
exec "+" a1 a2 = a1+a2
exec "-" a1 a2 = a1-a2
exec "*" a1 a2 = a1*a2
exec "/" a1 a2 = a1/a2
exec "^" a1 a2 = a1**a2

-- Является ли лексема числом

isNum :: String -> Bool
isNum s = (head s) `elem` "0123456789"

-- Выполнить накопленные операции до левой скобки

searchP :: [String] -> [Double] -> ([String],[Double])
searchP (o:os) ns | (o=="(") = (os,ns)
                  | otherwise = searchP os nn
                    where a2 = head ns
                          a1 = head $ drop 1 ns
                          v  = exec o a1 a2
                          nn = v : (drop 2 ns)                          

-- Вычисление                          
                          
calc' :: [String] -> [String] -> [Double] -> Double -> Double
calc' [] sO sN _ = empty sO sN
calc' (l:ls) sO sN arg | (isNum l) = calc' ls sO ((read l):sN) arg
                       | (l=="x") = calc' ls sO (arg:sN) arg
                       | (l=="(")  = calc' ls (l:sO) sN arg
                       | (l==")")  = calc' ls (fst r) (snd r) arg
                       | (null sO) = calc' ls (l:sO) sN arg
                       | (prty l) > (prty (head sO)) = calc' ls (l:sO) sN arg
                       | otherwise = calc' ls (l:(drop 1 sO)) (v:(drop 2 sN)) arg                  
                         where r = searchP sO sN
                               a2 = head sN
                               a1 = head $ drop 1 sN
                               v  = exec (head sO) a1 a2                           

azero :: [String] -> [String] -> [String]
azero [x] acc = if (head acc) == "-" then ["0"]++acc++[x] else acc++[x]
azero (x:y:xs) acc | x=="(" && y=="-" = azero xs (acc++["("]++["0"]++["-"])
                   | otherwise = azero (y:xs) (acc++[x])                          
                           
-- "Парадная" функция                         

calc :: String -> Double -> Double
calc formula arg = calc' (azero (parse formula "") []) [] [] arg

repl lex arq val = map (\ l -> if(l == arq) then val else l) lex
  
tabf :: String -> Double -> Double -> Double -> [Double]
tabf form a b k = map  (\ x -> calc' pf [] [] x) [a, a+dx .. b]
     where dx = (b-a)/k
           pf = azero (parse form "") []
           
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
norm n ys = map (\ y -> truncate $ (ymax-y)/dy) ys
          where ymax = (maximum ys)
                ymin = (minimum ys)
                dy   = (ymax-ymin)/(fromIntegral n)

draw :: [String] -> [Int] -> Int -> [String]
draw scr [] _ = scr
draw scr (y:ys) m = draw (setAst scr y m) ys (m+1)

graph n m form a b = draw (mkScr n m) (norm n (tabf form a b (fromIntegral m))) 0

main = do
--       [form,sa,sb] <- getArgs
--       putStrLn sa
--       putStrLn sb
--       putStrLn form
       putStr "formula: "
       hFlush stdout
       form <- getLine
       putStr "Xmin: "
       hFlush stdout
       sa <- getLine
       putStr "Xmax: "
       hFlush stdout
       sb <- getLine
       mapM putStrLn $ graph 25 80 form (read sa) (read sb)
       return ()
       
