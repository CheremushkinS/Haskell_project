data Ra =  R Integer Integer

gcd' :: Integer -> Integer -> Integer
gcd' n m | m>n = gcd' m n
         | m==0 = n
         | otherwise = gcd' m (n `mod` m)
         
instance Show Ra where
   show (R n d) = (show n) ++ "/" ++ (show d)         
         
instance Num Ra where
  (+) (R n1 d1) (R n2 d2) = (R n d)
      where nn=n1*d2+n2*d1
            dd=d1*d2
            g=gcd' nn dd
            n=nn `div` g
            d=dd `div` g                  
            
  (-) (R n1 d1) (R n2 d2) = (R n d)
      where nn=n1*d2-n2*d1
            dd=d1*d2
            g=gcd' nn dd
            n=nn `div` g
            d=dd `div` g                  

  (*) (R n1 d1) (R n2 d2) = (R n d)
      where nn=n1*n2+d1*d2
            dd=d1*d2
            g=gcd' nn dd
            n=nn `div` g
            d=dd `div` g                  

  abs (R n d) = (R (abs n) (abs d))

  fromInteger n = (R n 1)   
   
  signum (R n d) | n == 0 = 0
                 | n < 0  = -1
                 | otherwise = 1
                 
instance Eq Ra where
   (==) (R n1 d1) (R n2 d2) = (n1*d2)==(n2*d1)                 

--instance Ord Ra where
--   (>) (R n1 d1) (R n2 d2) = (n1*d2) > (n2*d1)   