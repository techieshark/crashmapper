module MyUtils (arrRange, mask, pad) where

-- arrRange and mask: 
-- a couple simple array helper functions to get a range of values from 
-- an array or to mask off all but a given set of indices.

-- arrRange arr (x1,x2) returns arr[x1..x2]. 
arrRange :: [a] -> (Int, Int) -> [a]
arrRange as (r1, r2) = take (r2-r1+1) (drop r1 as)


-- Given an array of a's and an array of integers, 
-- mask will return just the a's whose index is listed in the indices 
-- ('is') array.
mask :: [a] -> [Int] -> [a]
mask as is = map (as !!) is
-- result is as[i0..in] [as !! i | i <- is]  TODO

-- pad a given string s so that if it is less than n characters long, 
-- it is left padded with '0's. 
pad :: String -> Int -> String
pad s n
    | length s >= n = s
    | length s < n  = (take padwidth zeros) ++ s
                      where 
                      padwidth = n - (length s)
                      zeros = ['0', '0' ..] 

