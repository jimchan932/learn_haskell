module Ch13 where
import Data.List 
import Data.Tuple 
import Prelude
import RomanNums

allEqual :: Eq a => a -> a -> a -> Bool
allEqual a b c = a==b && a==c
lookupFirst :: Eq a => [(a,b)] -> a -> [b]
lookupFirst ws x = [z | (y,z)<-ws, y==x]
 
oneLookupFirst :: Eq a => [(a,b)] -> a -> b
oneLookupFirst [] x = error "empty list"
oneLookupFirst ((x,y):ys) a 
  | x==a = y
  | otherwise = oneLookupFirst ys a  
 
oneLookupSecond :: Eq b => [(a,b)] -> b -> a
oneLookupSecond ws = oneLookupFirst (map swap ws)  

numEqual :: Eq a => [a] -> a -> Int
numEqual xs x = length [z | z<-xs, z==x]

{-
member :: Eq a => [a] -> a -> Bool
member xs x = 
  case numEqual xs x of 
    0 -> False;
	_ -> True
-}
	
class Info a where
  examples :: [a] 
  size :: a -> Int

instance Info Bool where
  examples = [True,False]
  size _ = 1  

instance Info Char where
  examples = ['a','A','z','Z','0','9']
  size _   = 1
  
instance Info Int where
  examples = [-100..100] 
  size _ = 1    
  
data Shape = Rectangle Float Float | Circle Float deriving (Show,Eq)
			 
area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle h w) = h*w 

instance Info Shape where
  examples = [Rectangle 2 3,Circle 3.14]
  size = round.area 
  
instance Info a => Info [a] where
  examples = [[]]++[[x] | x<-examples]++[[x,y] | x<-examples,y<-examples] 
  size = foldr (+) 1.map size

instance (Info a,Info b) => Info (a,b) where
  examples = [ (x,y) | x<-examples , y<-examples ]
  size (x,y) = size x+size y+1

class Visible a where
  toString :: a -> String
  size2  :: a -> Int

instance Visible Char where
  toString ch = [ch]
  size2 _      = 1

instance Visible Bool where
  toString True = "True"
  toString False = "False"
  size2 _         = 1

instance Visible a => Visible [a] where
  toString = concatMap toString
  size2 = foldr (+) 1 . map size2
  
{-
class Eq a where
  (==),(/=) :: a -> a -> Bool
  x /= y = not (x==y)
  x == y = not (x/=y)
  
class Eq a => Ord a where
  (<), (<=), (>), (>=) : : a -> a -> Bool
  max, min :: a -> a -> a
  compare : : a -> a -> Ordering 
  x <=y = x<y || x==y
  x<y = y>x  
-}  
class (Ord a,Visible a) => OrdVis a where 
  vSort :: [a] -> String   
  vSort = toString.iSort

class Checkable b where
  infoCheck :: Info a => (a -> b) -> Bool

instance Checkable Bool where
  infoCheck = and.(`map` examples)
  
instance (Info a, Checkable b) => Checkable (a -> b) where
  infoCheck prop = and (map (infoCheck.prop) examples) 

prop_rev :: [Int] -> Bool  
prop_rev xs = --reverse (xs++ys) == reverse ys ++ reverse xs  
  xs == reverse (reverse xs)  
  
type ShowS = String -> String
{-
instance (Eq a,Eq b) => Eq (a,b) where
  (x,y) == (z,w) = x==z && y==w

instance (Ord a,Ord b) => Ord (a,b) where
  (x,y) < (z,w) = x < z || xs < ys
 
instance Ord b => Ord [b] where   
  (x:xs) < (y:ys) = x <y || (x==y && xs < ys)  
-}

instance (Visible a,Visible b) => Visible (a,b) where
  toString (a,b) = toString a++toString b
  size2 (a,b) = size2 a+size2 b+1  
  
instance (Visible a,Visible b,Visible c) => Visible (a,b,c) where
  toString (a,b,c) = toString a++toString b++toString c
  size2 (a,b,c) =  size2 a+size2 b+size2 c+1  

myCompare :: (Visible a,Ord a,Visible b,Ord b) => a -> b -> Bool 
myCompare x y = size2 x <= size2 y

instance (Info a, Info b, Info c) => Info (a,b,c) where
  examples = [(x,y,z) | x<-examples, y<-examples, z<-examples]
  size (a,b,c)= size a+size b+size c+1 
   
data Move = Rock | Paper | Scissors deriving (Show,Eq)
  
instance Info Move where 
  examples = [Rock,Paper,Scissors]
  size _ = 1
    
{- 
class Show a where
  showPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
-}

class OrdData a where
  startFrom :: a -> [a]
  startTo :: a -> [a]

beat :: Move -> Move	
beat Rock       = Paper
beat Scissors	= Rock
beat _          = Scissors

instance OrdData Move where
  startFrom x = [x,beat x,beat (beat x)]
  startTo = reverse.startFrom
  
instance (Info a, Show b) => Show (a -> b) where
  show = show.(`map` examples)

f :: (a, Char) -> (a, [Char])
f (x,y) = (x,['a'..y])

g :: (Int, [b]) -> Int
g (m,zs) = m+length zs

h :: (Int, Char) -> Int
h = g.f

{- map :: (a -> b) -> [a] -> [b]
map Circle :: [Float] -> [Shape]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s [] = s
foldr f s (x:xs) = f x (foldr f s xs)
-}

ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]  
ins x (y:ys) = if x <= y then x:y:ys else y:ins x ys

iSort :: Ord a => [a] -> [a]
iSort = foldr ins []

{- 
(Int -> b)
(a -> Bool)     (Int -> Bool)

(Int,a,a)
(a,a,[Bool])    X 
                a = Int 
				a = [Bool]
				Int /= [Bool]

(Bool,[Bool]) E (a,[a])^(Bool,[Bool]) E (b,c)           
		
f :: [a] -> [b] -> a -> b

f [] [] :: a -> a

h :: a -> a -> a
h x = f x x 
-}
zircon = zircon
{-
curry id :: a -> b -> (a,b)
uncurry id :: (b -> c,b) -> c
curry (curry id) :: a -> b -> b1 -> ((a,b),b1)
uncurry (uncurry id) :: (a -> b -> c, a) -> c
uncurry curry :: ((a,b) -> c,d) -> b -> c

-}

member :: Eq a => [a] -> a -> Bool
member [] y = False
member (x:xs) y = x==y || member xs y

-- e :: Ord b => [[b]]
-- member e :: Ord b => [b] -> Bool

merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = []  
merge (x:xs) (y:ys)
  | x<y       = x: merge xs (y:ys)
  | x==y      = x: merge xs ys
  | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:xs) = compM [x] (mergeSort xs)  

compM :: Ord a => [a] -> [a] -> [a]
compM [] [] = [] 
compM (x:xs) (y:ys) =
  case compare x y of 
    GT -> y:compM (x:xs) ys;
    EQ -> x:compM xs ys;
    _  -> x:compM xs (y:ys);
compM xs [] = xs
compM [] ys = ys
    	
mult x y = x*y
divide x = x `div` 2
share x = x/2.0
