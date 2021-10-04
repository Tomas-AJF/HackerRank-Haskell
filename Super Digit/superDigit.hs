import Data.Char 

{-************************************************************** 
  _______                __                     _ ______       *
 |__   __|              /_/           /\       | |  ____|      *
    | | ___  _ __ ___   __ _ ___     /  \      | | |__         *
    | |/ _ \| '_ ` _ \ / _` / __|   / /\ \ _   | |  __|        *
    | | (_) | | | | | | (_| \__ \  / ____ \ |__| | |           *
    |_|\___/|_| |_| |_|\__,_|___/ /_/    \_\____/|_|           *
                                                               *
           https://github.com/Tomas-AJF                        *                     
                                                               *                                                  
***************************************************************-}


super_digit list_of_numbers = find_super_digit $ multiply
           where         
             find_super_digit list_of_numbers_to_sum@(x:xs)
                   | length (list_of_numbers_to_sum) > 1   = find_super_digit $ show $ sum $ map digitToInt list_of_numbers_to_sum
                   | otherwise             = list_of_numbers_to_sum
                         
             multiply          =   show (extensive_number * short_number)                    
             extensive_number  =   read (head list_of_numbers)  :: Integer
             short_number      =   read $ head $ tail list_of_numbers :: Integer
             
                  
main :: IO ()
main = interact $ super_digit . words
