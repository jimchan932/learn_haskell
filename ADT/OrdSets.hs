module OrdSets ( 
  Set,
  empty                    , -- Set a
  sing                     , -- a -> Set a
  memSet                   , -- Ord a => Set a -> a -> Bool
  union,inter,diff,symDiff , -- Ord a => Set a -> Set a -> Set a
  subset                   , -- Eq a => Set a -> Set a -> Bool 
  properSub,properSuper    , -- Ord a => Set a -> Set a -> Bool
  setDiff,setInter,setUnion, -- Ord a => Set (Set a) -> Set a 
  product                  , -- (Ord a,Ord b) => Set a -> Set b -> Set (a,b) 
  makeSet                  , -- Ord a => [a] -> Set a
  mapSet                   , -- Ord b => (a -> b) -> Set a -> Set b
  filterSet                , -- (a -> Bool) -> Set a -> Set a
  foldSet                  , -- (a -> a -> a) -> a -> Set a -> a
  showSet                  , -- (a -> String) -> Set a -> String
  card                     , -- Set a -> Int
  flatten                  , -- Set a -> [a]
  ) where
import Data.List.Ordered (nubSort)
import Test.QuickCheck 
import Prelude hiding (product)

newtype Set a = Set [a] deriving (Show,Eq ) 
{-
instance Eq a => Eq (Set a) where 
  (==) = subset-}
  
prop_eq :: Eq a => [a] -> [a] -> Bool  
prop_eq xs ys = subset (Set xs) (Set ys) == ((Set xs) == (Set ys))  
   
instance Ord a => Ord (Set a) where 
  (<=) = leqSet   

instance Monad Set where 
  return x       = Set [x]
  fail _         = empty 
  (Set xs) >>= f = Set (concat [xss | Set xss <- map f xs])
  
empty = Set [] 

sing :: a -> Set a 
sing x = Set [x] 

memSet :: Ord a => Set a -> a -> Bool
memSet (Set s@(x:xs)) y  
  | x<y             = memSet (Set xs) y
  | x==y            = True
  | x>y || s==[] = False 
  
union :: Ord a => Set a -> Set a -> Set a 
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs 
uni (x:xs) (y:ys)
  | x<y  = x:uni xs (y:ys)
  | x==y = x:uni xs ys
  | x>y  = y:uni (x:xs) ys
  
inter :: Ord a => Set a -> Set a -> Set a 
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int xs [] = xs 
int [] ys = ys 
int a@(x:xs) b@(y:ys) 
  | x<y            = int xs b
  | x==y           = x:int xs ys 
  | x>y            = int a ys
  
diff :: Ord a => Set a -> Set a -> Set a 
diff (Set xs) (Set ys) = Set ( diff' xs ys )  

diff' :: Ord a => [a] -> [a] -> [a]
diff' xs [] = xs
diff' [] ys = []
diff' (x:xs) (y:ys)
  | x==y = diff' xs ys 
  | x<y  = x:diff' xs (y:ys)
  | x>y  = x:diff' xs ys       
 
symDiff :: Ord a => Set a -> Set a -> Set a 
symDiff (Set xs) (Set ys) = Set (diff' xs ys ++ diff' ys xs)  

subset :: Eq a => Set a -> Set a -> Bool 
subset (Set xs) (Set ys) = xs==ys

properSub :: Ord a => Set a -> Set a -> Bool
properSub (Set xs) (Set ys) = properS xs ys 

properS :: Ord a => [a] -> [a] -> Bool
properS (x:xs) (y:ys)
  | x<y  = False 
  | x==y = properS xs ys
  | x>y  = properS (x:xs) ys   
properS [] _ = True 
porperS _ [] = False  
 
properSuper :: Ord a => Set a -> Set a -> Bool
properSuper = flip properSub
   
product :: (Ord a,Ord b) => Set a -> Set b -> Set (a,b)
product (Set xs) (Set ys) = Set (prod xs ys)    

prod :: (Ord a,Ord b) => [a] -> [b] -> [(a,b)]
prod xs ys = [(x,y) | x<-xs, y<-ys] 

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = xs<=ys    
   
makeSet :: Ord a => [a] -> Set a 
makeSet = Set . nubSort

mapSet :: Ord a => (a -> b) -> Set a -> Set b 
mapSet f (Set xs) = Set (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a 
filterSet p (Set xs) = Set (filter p xs) 

foldSet :: (a -> a -> a) -> a -> Set a -> a 
foldSet f x (Set xs) = foldr f x xs 

showSet :: (a -> String) -> Set a -> String 
showSet f (Set xs) = unlines (map f xs)  

card :: Set a -> Int
card (Set xs) = length xs 

setUnion,setDiff,setInter :: Ord a => Set (Set a) -> Set a 

setUnion = foldSet union (Set [])

setInter = foldSet inter (Set [])  

setDiff = foldSet diff (Set [])  

flatten (Set xs) = xs