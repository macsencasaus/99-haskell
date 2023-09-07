module Main where

import Data.Char
import Data.List

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 0 = x
elementAt (_:xs) i = elementAt xs (i-1)

myLength :: [a] -> Int
myLength [x] = 1
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (first:rest) = first == last rest && isPalindrome (init rest)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)


pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, last) = span (==x) xs in (x:first) : pack last

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

encodeModified :: (Eq a, Show a) => [a] -> [[Char]]
encodeModified xs = map (\(l, x) -> (if l==1 then "Single" else "Multiple " ++ show l) ++ " " ++ show x) (encode xs)

-- decodeModified :: [[Char]] -> [Char]
-- decodeModified [] = ""
-- decodeModified ([_, l, x]:xs) = replicate (digitToInt l) x ++ decodeModified xs
-- decodeModified ([_, x]:xs) = x : decodeModified xs

encodeDirect :: [Char] -> [[Char]]
encodeDirect [] = []
encodeDirect (x:xs) 
    | length (x:first) == 1 = ("Single " ++ show x) : encodeDirect last 
    | otherwise = ("Multiple " ++ show (length (x:first)) ++ " " ++ show x) : encodeDirect last
    where
        (first,last) = span (==x) xs

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli x i = concatMap (replicate i) x

dropEvery :: [a] -> Int -> [a]
dropEvery x a
    | null x = []
    | length x < a = x
    | otherwise = first ++ dropEvery (drop a x) a
    where
        first = init (take a x)

split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split xs 0 = ([], xs)
split (x:xs) 1 = ([x], xs)
split (x:xs) n
    = (x:first, last)
    where (first, last) = split xs (n-1)


slice :: [a] -> Int -> Int -> [a]
slice _ _ 0 = []
slice xs 1 b = head xs : slice (tail xs) 1 (b-1)
slice xs a b = slice (tail xs) (a-1) (b-1)

rotate :: [a] -> Int -> [a]
rotate xs n 
    | n < 0 = rotate xs $ length xs + n
    | n == 0 = xs
rotate (x:xs) n = rotate (xs ++ [x]) $ n-1

removeAt :: Int -> [a] -> (a,[a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs)
    = (a, x:b)
    where (a,b) = removeAt (n-1) xs

insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y:xs
insertAt y (x:xs) n = x : insertAt y xs (n-1)

range :: Int -> Int -> [Int]
range a b
    | a == b = [a]
    | otherwise = a : range (a+1) b 

main :: IO ()
main = do
    let n = 3
        m = 7
    let arr = [1,2,3,4,5,6,7,8,9,10,11] :: [Int] 
    let arrStr = "aaaaabbbbbcaddd" :: [Char]
    let nestedList = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

    print $ "myLast: " ++ show (myLast arr)
    print $ "myButLast: " ++ show (myButLast arr)
    print $ "elementAt: " ++ show (elementAt arr n)
    print $ "myLength: " ++ show (myLength arr)
    print $ "myReverse: " ++ show (myReverse arr)
    print $ "isPalindrome: " ++ show (isPalindrome arr)
    print $ "flatten: " ++ show (flatten nestedList)
    print $ "compress: " ++ show (compress arrStr)
    print $ "pack: " ++ show (pack arrStr)
    print $ "encode: " ++ show (encode arrStr)
    print $ "encodeModified: " ++ show (encodeModified arrStr)
    -- print $ "decodeModified: " ++ show (decodeModified $ encodeModified arrStr)
    print $ "encodeDirect: " ++ show (encodeDirect arrStr)
    print $ "dupli: " ++ show (dupli arr)
    print $ "repli: " ++ show (repli arr n)
    print $ "dropEvery: " ++ show (dropEvery arr n)
    print $ "split: " ++ show (split arr n)
    print $ "slice: " ++ show (slice arr n m)
    print $ "rotate: " ++ show (rotate arr n)
    print $ "removeAt: " ++ show (removeAt n arr)
    print $ "insertAt: " ++ show (insertAt m arr n)
    print $ "range: " ++ show (range n m)