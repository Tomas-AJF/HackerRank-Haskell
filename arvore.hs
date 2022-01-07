module Main where

data TTT aa = Leaf [aa] | Node (TTT aa) (TTT aa)  deriving (Show,Eq)


t0 = tree      
t1 = ff_a tree
 
main :: IO ()
main = do
    print t0
    print t1
----------------------------------------------------------------------

-- Compute the square of its argument
a :: Integer -> Integer 
a n = n^2

-- Compute its argument modulo 5
b :: Integer -> Integer
b n = mod n 2

-- Compute the sum of list n
c :: Integer -> Integer
c n = sumNumbers [0 .. n] 
               where 
                   sumNumbers (x:xs) = x + sumNumbers xs
                   sumNumbers []     = 0 

----------------------------------------------------------------------


ff_a :: TTT Integer -> TTT Integer; 
ff_a t = mm a t


ff_b :: TTT Integer -> TTT Integer; 
ff_b t = mm b t


ff_c :: TTT Integer -> TTT Integer; 
ff_c t = mm c t


mm :: (Integer -> Integer) -> TTT Integer -> TTT Integer 
mm f (Node x y) = Node (mm f x) (mm f y) 
mm f (Leaf n) = Leaf (apply_f f n)


apply_f :: (Integer -> Integer) -> [Integer] -> [Integer]
apply_f f (t:ts) = f t : apply_f f ts
apply_f f [] = []



tree =   
    Node   
        (Node   
            (Node   
                ((Leaf[2 ,  3,  5, 7 ]))
                ((Leaf[11, 13, 17, 19]))
            )  
            (Node   
                ((Leaf[101, 103, 107]))
                ((Leaf[109, 113, 127]))
            )  
        )
          
        (Node   
            (Node   
                ((Leaf[131, 137, 139]))
                ((Leaf[149, 151, 157]))
            )  
            (Node   
                ((Leaf[167, 173, 179]))
                ((Leaf[181, 191, 193]))
            )  
        )  
        
        
        
        
        




