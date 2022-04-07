module BinarySearch where

import Data.Maybe
import Data.List

search n  [x] 
             | n == x    = Just n
             | otherwise = Nothing

search n list@(_)
                  | n == throwMid = Just (throwMid)
                  | n  < throwMid = search n splitListLeft 
                  | n  > throwMid = search n splitListRight
                  | otherwise     = error "ERROR" 
                   where
                         sortedList = sort $ list
                         sizeOfList = fromIntegral $ length $ sortedList
                         findMid    = round $ (/) sizeOfList 2 
                         throwMid   = (!!) sortedList (findMid-1)
                         
                         splitListRight  = snd $ splitAt findMid sortedList
                         
                         splitListLeft 
                                      | (findMid-1) == 0 =  fst $ splitAt (findMid  )  sortedList
                                      | otherwise        =  fst $ splitAt (findMid-1)  sortedList
