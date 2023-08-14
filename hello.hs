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
compress (x:y:xs) = if x == y then compress (y:xs) else x : compress (y:xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, last) = span (==x) xs in (x:first) : pack last

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

encodeModified :: (Eq a, Show a) => [a] -> [[Char]]
encodeModified xs = map (\(l, x) -> (if l==1 then "Single " else "Multiple " ++ show l) ++ " " ++ show x) (encode xs)

-- decodeModified :: [[Char]] -> [a]
-- decodeModified xs = let [_, l, x] = splitOn " " xs in []

main :: IO ()
main = do
    let arr = [1,2,3,4,5] :: [Int] 
    print $ "myLast: " ++ show (myLast arr)
    print $ "myButLast: " ++ show (myButLast arr)