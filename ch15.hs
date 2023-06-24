module Ch15 (main) where 
import Types 
import Coding
import MakeCode 
import Test.QuickCheck (quickCheck)

main = print decoded

-- the example message to be coded 
message :: String 
message = "there are green hills here"
 
-- the Huffman tree generated from the example
treeEx :: Tree 
treeEx = codes "there is a green hill" 

-- the coding table generated from the example
tableEx :: Table 
tableEx = codeTable (codes "There is a green hill")

-- the example in code
coded :: HCode 
coded = codeMessage tableEx message

-- the example coded and then decoded
decoded :: String
decoded = decodeMessage treeEx coded 

mergeDuplicate :: Ord a => [a] -> [a] -> [a]
mergeDuplicate xs [] = xs
mergeDuplicate [] ys = ys
mergeDuplicate (x:xs) (y:ys)
  | x<=y      = x:merge xs (y:ys)  
  | otherwise = y:merge (x:xs) ys
  
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x==y      = x:merge xs ys 
  | x<y       = x:merge xs (y:ys)  
  | otherwise = y:merge (x:xs) ys

{-compareMerge :: Ord a => [a] -> [a] -> [a]
compareMerge xs [] = xs
compareMerge [] ys = ys
compareMerge (x:xs) (y:ys) =
  case compare x y of 
	LT -> x:compareMerge xs (y:ys);
    GT -> y:compareMerge (x:xs) ys;	
	EQ -> compareMerge xs (y:ys)-}
	
prop_coding :: String -> Bool	
prop_coding [] = True 
prop_coding [x] = True -- exception in "a", since it does not have a Tree
prop_coding str = str == decodeMessage tree (codeMessage (codeTable tree) str)
  where 
  tree = codes str
