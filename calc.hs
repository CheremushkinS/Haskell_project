
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
                          
calc' :: [String] -> [String] -> [Double] -> Double
calc' [] sO sN = empty sO sN
calc' (l:ls) sO sN | (isNum l) = calc' ls sO ((read l):sN)
                   | (l=="(")  = calc' ls (l:sO) sN
                   | (l==")")  = calc' ls (fst r) (snd r)
                   | (null sO) = calc' ls (l:sO) sN
                   | (prty l) > (prty (head sO)) = calc' ls (l:sO) sN
                   | otherwise = calc' ls (l:(drop 1 sO)) (v:(drop 2 sN))                  
                     where r = searchP sO sN
                           a2 = head sN
                           a1 = head $ drop 1 sN
                           v  = exec (head sO) a1 a2                           

-- "Парадная" функция                         

calc :: String -> Double
calc formula = calc' (parse formula "") [] []
