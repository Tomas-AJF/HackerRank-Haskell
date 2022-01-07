module Main where

{------------------------------------------------------------------------------------------------------------
Define in HASKELL a data type TTT aa of a tree with each vertex having 0,1 or 2 children, 
and whose every leaf (a vertex with 0 children) and every internal vertex has a list of values of aa type. 
-------------------------------------------------------------------------------------------------------------}

data TTT aa = Leaf [aa] | Node (TTT aa) (TTT aa)  deriving (Show,Eq)
 
-- ********** Compute the square of its argument ********** --
a :: Integer -> Integer 
a n = n^2

-- ********** Compute its argument modulo 5 ********** --
b :: Integer -> Integer
b n = mod n 2

-- ********** Compute the sum of list n ********** --
c :: Integer -> Integer
c n = sumNumbers [0 .. n] 
               where 
                   sumNumbers (x:xs) = x + sumNumbers xs
                   sumNumbers []     = 0 
                   
{--------------------------------------------------------------------------------------------------------
Apply the function mm to the argument functions a, b and c to receive functions ff_a, ff_b and ff_c 
that apply the functionality of the respectively a, b and c functions to all members of all lists 
in the vertexes of their single TTT Integer type argument. Demonstrate the usage of ff_a, ff_b and ff_c
functions on an example tree containing at least 5 vertexes with non-empty lists,
altogether at least 12 different numbers, including both the positive and negative ones.
------------------------------------------------------------------------------------------------------------}
ff_a :: TTT Integer -> TTT Integer; 
ff_a t = mm a t


ff_b :: TTT Integer -> TTT Integer; 
ff_b t = mm b t


ff_c :: TTT Integer -> TTT Integer; 
ff_c t = mm c t


{--------------------------------------------------------------------------------------------------------
Define a function mm that takes as its first argument a function f:aa -> aa 
and as the second argument a TTT aa type tree x and as the result returns a TTT aa type tree
obtained from the input tree x by applying f to every element of every list placed in the x vertex.
---------------------------------------------------------------------------------------------------------}

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
       
