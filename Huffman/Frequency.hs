module Frequency (frequency, mergeSort,freq) -- [Char] -> [(Char,Int)]
where 
import Types
import Test.QuickCheck hiding (frequency)
import Queue2

mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort _ [] = error "empty list"
mergeSort merge xs
  | length xs < 2 = xs 
  | otherwise     = merge (mergeSort merge first)
                          (mergeSort merge second)
	    where
		first  = take half xs
		second = drop half xs
		half   = (length xs) `div` 2
  
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x>=y      = x:merge xs (y:ys)  
  | otherwise = y:merge (x:xs) ys
    
sort :: Ord a => [a] -> [a]  
sort = mergeSort merge

sorted :: Ord a => [a] -> Bool
sorted xs = xs == sort xs

prop_freq :: [Char] -> Bool  
prop_freq [] = True
prop_freq xs = (sorted.map (\(Leaf _ x) -> x).frequency) xs 
		
alphaMerge :: [Tree] -> [Tree] -> [Tree]
alphaMerge [] ys = ys 
alphaMerge xs [] = xs
alphaMerge (Leaf c1 n:xs) (Leaf c2 m:ys) 
  | c1==c2    = Leaf c1 (n+m):alphaMerge xs ys
  | c1<c2     = Leaf c2 m:alphaMerge (Leaf c1 n:xs) ys
  | otherwise = Leaf c1 n:alphaMerge xs (Leaf c2 m:ys)

freqMerge :: [Tree] -> [Tree] -> [Tree]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge (Leaf c1 n:xs) (Leaf c2 m:ys)
  | (n>m || (n==m && c1>c2)) 
    = Leaf c1 n:freqMerge xs (Leaf c2 m:ys)
  | otherwise         
	= Leaf c2 m:freqMerge (Leaf c1 n:xs) ys	

frequency :: [Char] -> [Tree]

frequency = mergeSort freqMerge . mergeSort alphaMerge . map start
  where 
  start c = Leaf c 1
	 
freq = mergeSort fMerge . mergeSort aMerge . map start
  where 
  start c = Leaf c 1
  
aMerge :: [Tree] -> [Tree] -> [Tree]
aMerge [] ys = ys 
aMerge xs [] = xs
aMerge (Leaf c1 n:xs) (Leaf c2 m:ys) 
  | c1==c2    = Leaf c1 (n+m):aMerge xs ys
  | c1>c2     = Leaf c2 m:aMerge (Leaf c1 n:xs) ys
  | otherwise = Leaf c1 n:aMerge xs (Leaf c2 m:ys)
  
fMerge :: [Tree] -> [Tree] -> [Tree]
fMerge xs [] = xs
fMerge [] ys = ys
fMerge (Leaf c1 n:xs) (Leaf c2 m:ys)
  | (n<m || (n==m && c1<c2)) 
    = Leaf c1 n:fMerge xs (Leaf c2 m:ys)
  | otherwise         
	= Leaf c2 m:fMerge (Leaf c1 n:xs) ys		 