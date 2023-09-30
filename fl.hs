
fl :: (a -> b -> a) -> a -> [b] -> a
fl _ acc [] = acc
fl f acc (x:xs) = fl f (f acc x) xs

fr :: (a -> b -> b) -> b -> [a] -> b
fr _ acc [] = acc
fr f acc (x:xs) = f x (fr f acc xs)

--Main> fr (\ z acc -> "("++acc++"*"++(show z)++")") "1" [1,2,3,4,5]
--"(((((1*5)*4)*3)*2)*1)"

--Main> fl (\ acc z -> "("++acc++"*"++(show z)++")") "1" [1,2,3,4,5]
--"(((((1*1)*2)*3)*4)*5)"   