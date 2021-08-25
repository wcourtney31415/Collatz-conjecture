module Lib
  ( 
    collatz,
  )
where

collatz :: Int -> IO ()
collatz number =
  let collatzR :: [Int] -> IO ()
      collatzR list =
        let previous = last list
            longEnough = length list >= 3
            previousThree = take 3 (reverse list)
            foundPattern = previousThree == [1, 2, 4]
         in if longEnough && foundPattern
              then print list
              else
                if odd previous
                  then
                    let next = previous * 3 + 1
                        newList = list ++ [next]
                     in collatzR newList
                  else
                    let 
                        next = floor $ fromIntegral previous / 2
                        newList = list ++ [next]
                     in collatzR newList
   in collatzR [number]
