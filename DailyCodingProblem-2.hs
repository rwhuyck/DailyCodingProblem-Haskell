module DailyCodingProblem2(main) where

    {-
    This problem was asked by Uber.

    Given an array of integers, return a new array such that
    each element at index i of the new array is the product
    of all the numbers in the original array except the one at i.

    For example, if our input was [1, 2, 3, 4, 5], the expected output 
    would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], 
    the expected output would be [2, 3, 6].

    Follow-up: what if you can't use division? -- Not yet, apparently.
    -}
 
    input1 = [1, 2, 3, 4, 5]
    input2 = [3, 2, 1]
    
    makeProduct :: [Int] -> [Int]
    makeProduct xs = [(product xs) `div` x | x <- xs]

    result = putStrLn $ show (makeProduct input1) ++ "\n" ++ show (makeProduct input2)
    
    main = result
