module Ch14_2 where
import Data.List hiding ((!!))
import Prelude hiding (either,maybe,(!!))
import Data.Tuple
data Pairs a = Pr a a deriving Show

equalPair :: Eq a => Pairs a -> Bool
equalPair (Pr x y) = x==y

infixr 5 :::

data List a = NilL | a ::: (List a)
              deriving (Eq,Ord,Show,Read)
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show		  
			  
depth Nil = 0
depth (Node _ t1 t2) = 1+ max (depth t1) (depth t2)

occurs :: Eq a => Tree a -> a -> Integer
occurs Nil _ = 0
occurs (Node n t1 t2) p 
  | n==p = 1+occurs t1 p+occurs t2 p
  | True = occurs t1 p+occurs t2 p
  
collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node a t1 t2) = collapse t1++[a]++collapse t2  

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

--data Either a b = Left a | Right b deriving (Eq,Ord,Read,Show)

isLeft :: Either a b -> Bool
isLeft (Left _) = True				  
isLeft (Right _) = False

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right x) = g x
{-
applyLeft :: (a -> c) -> Either a b -> c
applyLeft f (Left x) = f x
applyLeft f (Right x) = error "applyLeft is applied to Right"
-}
sortTrees :: Ord a => Tree a -> [a]
sortTrees = sort.collapse

twist :: Either a b -> Either b a
twist (Right x) = Left x
twist (Left x) = Right x		  
{-
prop_twist :: Either a b -> Bool		  
prop_twist x = twist $ twist x == id x-}

applyLeft :: (a -> c) -> Either a b -> c
applyLeft f = either f (error "applyLeft is applied to Right") 

toLeft :: (a -> b) -> Either a b -> Either b a
toLeft f = \(Left x) -> Left (f x)

join :: (a -> c) -> (b -> c) -> Either a b -> Either c c
join f g (Left x) = Left (either f g (Left x))
join f g (Right x) = Right (either f g (Right x))

data GTree a = Leaf a | GNode [GTree a] deriving Show

leaves :: GTree a -> Int
leaves (GNode []) = 0
leaves (Leaf _) = 1
leaves (GNode (x:xs)) = leaves x+leaves (GNode xs) 

sumTree' :: GTree Int -> Int
sumTree' (Leaf n) = n
sumTree' (GNode xs) = sum $ map (\(Leaf n) -> n) xs 
{-
depth :: GTree a -> Int
depth  
  where
  count (Leaf _) = 0
  count (gNode   
-}
elemTree' :: Eq a => a -> GTree a -> Bool
elemTree' p (Leaf x) = p==x
elemTree' p (GNode (x:xs)) = elemTree' p x || elemTree' p (GNode xs)

depthTree' :: GTree a -> Int
depthTree' (Leaf _) = 0
depthTree' (GNode xs) = 1+maximum (map depthTree' xs)  

mapTree' :: (a -> b) -> GTree a -> GTree b
mapTree' f (Leaf x) = Leaf (f x)
mapTree' f (GNode xs) = GNode (map (mapTree' f) xs)

flatten :: GTree a -> [a]
flatten (GNode []) = []
flatten (Leaf x) = [x]
flatten (GNode (x:xs)) = flatten x ++ flatten (GNode xs)

errDiv :: Integer -> Integer -> Maybe Integer
errDiv n m 
  | m/=0 = Just (div n m) 
  | True = Nothing

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x 

tMaybe1 = maybe 56 (+1) (mapMaybe (*3) (errDiv 9 0))
tMaybe2 = maybe 56 (+1) (mapMaybe (*3) (errDiv 9 1)) 

(!!) :: [a] -> Int -> Maybe a
(x:xs)!!0 = Just x
(x:xs)!!n = xs!!(n-1)
[]!!_= Nothing   

process :: [Int] -> Int -> Int -> Int 
process xs n m = maybe 0 (maybe (0+) (+) (getSize max)) (getSize min)
  where 
  getSize f = uncurry f (xs!!n,xs!!m)  
  
squashMaybe :: Maybe (Maybe a) -> Maybe a
squashMaybe (Just Nothing) = Nothing
squashMaybe (Just (Just x)) = Just x

composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe f g = squashMaybe.mapMaybe g.f
tCompose = composeMaybe (\y -> Just (2*y)) (\x -> Just x) 2  

data Err a = Error String | OK a deriving (Show,Eq,Ord)

mapErr :: (a -> b) -> Err a -> Err b 
mapErr f (Error s) = Error s
mapErr f (OK x) = OK (f x)

dealErr :: (a -> b) -> Err a -> b  
dealErr f (Error st) = error st
dealErr f (OK x) = f x

squashErr :: Err (Err a) -> Err a
squashErr (OK (OK x)) = OK x
squashErr (OK (Error s)) = Error s 

composeErr :: (a -> Err b) -> (b -> Err c) -> (a -> Err c)
composeErr f g = squashErr.mapErr g.f


data Inmess = No | Yes Arrival Service deriving (Eq,Show)
type Arrival = Integer  
type Service = Integer
		
data Outmess = Discharge Arrival Wait Service deriving (Eq,Show)
type Wait = Integer 

data Edit = Change Char | Swap | 
            Copy | Delete | 
			Insert Char | Kill deriving (Show,Eq)  
  
transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = [Kill]
transform [] ys = map Insert ys
transform (a:b:xs) (c:d:ys) | swap (a,b)==(c,d) = Swap:transform xs ys
transform (x:xs) (y:ys)
  | x==y = Copy:transform xs ys
  | otherwise =  foldr1 (\x y -> if cost x <=cost y then x else y) 
                     [Delete:transform xs (y:ys),
                      Insert y:transform (x:xs) ys,
					  Change y:transform xs ys]

cost :: [Edit] -> Int
cost = length.filter (/=Copy)

prop_transformLength :: String -> String -> Bool
prop_transformLength xs ys = 
    cost (transform xs ys) <= length ys+1
	
prop_transform :: String -> String -> Bool
prop_transform xs ys = edit (transform xs ys) xs == ys

edit :: [Edit] -> String -> String
edit [] st = st
edit (Change c:xs) (s:st) = c:edit xs st
edit (Swap:xs) (a:b:st) = b:a:edit xs st
edit (Swap:_) [x] = [x]
edit (Copy:xs) (s:st) = s:edit xs st
edit (Delete:xs) (s:st) = edit xs st
edit (Insert c:xs) st = c:edit xs st
edit (Kill:xs) st = []

prop_transformSegment :: String -> String -> Bool
prop_transformSegment (x:xs) (y:ys) = 
    if x==y then head (transform (x:xs) (y:ys)) == Copy
	else True

data ParkIn = Park CarCode Arrival Duration | Full
type CarCode = String 
type Duration = Integer

data Leave = In | Leave CarCode Arrival Duration 

data Efficiency = Car CarCode Fuel deriving (Show,Eq) 
type Fuel = Double 

instance Ord Efficiency where
  (Car _ e1) < (Car _ e2) = e1 < e2
  (Car _ e1) <= (Car _ e2) = e1 <= e2
  
data Vector = Vec Float Float 

class Movable a where 
  move :: Vector -> a -> a 
  reflectX :: a -> a 
  reflectY :: a -> a 
  reflect180 :: a -> a 
  reflect180 = reflectX.reflectY 
  scale :: Float -> a -> a 
  rotate :: Angle -> a -> a 
data Point = Point Float Float deriving Show

data Angle = Angle Float deriving Show 

instance Movable Point where
  move (Vec v1 v2) (Point c1 c2) = Point (v1+c1) (v2+c2)
  reflectX (Point c1 c2) = Point c1 (-c2)  
  reflectY (Point c1 c2) = Point (-c1) c2 
  scale n (Point x y) = Point (n*x) (n*y)
  rotate (Angle a) (Point x y) = Point (x * sin a+ y * cos a) (x*sin a + y*cos a) 
  
data Figure = Line Point Point | 
              Circle Point Float deriving Show 
			  
instance Movable Figure where 
  move v (Line p1 p2) = Line (move v p1) (move v p2)
  move v (Circle p r) = Circle (move v p) r
  reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2) 
  reflectX (Circle p r) = Circle (reflectX p) r 
  reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2) 
  reflectY (Circle p r) = Circle (reflectY p) r    
  scale n (Line x y) = Line (scale n x) (scale n y)
  rotate a (Line x y) = Line (rotate a x) (rotate a y)
     
instance Movable a => Movable [a] where 
  move v = map (move v)
  reflectX = map reflectX
  reflectY = map reflectY 
  rotate a = map (rotate a) 
  scale n = map (scale n)
  
class Named a where
  lookName :: a -> String 
  giveName :: String -> a -> a 
  
data Name a = Pair a String deriving (Show,Eq)

exam1 = Pair (Point 0.0 0.0) "Dweezil"
    
instance Named (Name a) where 
   lookName (Pair obj nm) = nm
   giveName nm (Pair obj _) = Pair obj nm

mapName :: (a -> b) -> Name a -> Name b 
mapName f (Pair obj nm) = Pair (f obj) nm

instance Movable  a => Movable (Name a) where 
  move v = mapName (move v) 
  reflectX = mapName reflectX
  reflectY = mapName reflectY 
  scale n = mapName (scale n)
  rotate a = mapName (rotate a)
  
class (Movable b, Named b) => NamedMovable b where
  makeName :: (b -> b) -> String -> Name b -> Name b 
  
instance Movable a => NamedMovable (Name a) where 
  makeName f st = mapName f.giveName st
 
instance Movable b => Movable (b,c) where 
  move v (x,y) = (move v x,y)
  reflectX (x,y) = (reflectX x,y)
  reflectY (x,y) = (reflectY x,y) 
  scale n (x,y) = (scale n x,y) 
  rotate a (x,y) = (rotate a x,y)

instance Named c => Named (b,c) where 
  giveName nm (x,n) = (x,giveName nm n)
  lookName (x,n) = lookName n
  {-
instance (Movable b,Named c) => NamedMovable (b,c) where 
  makeName f st (Pair (x,y) _) = Pair ($ move (x,makeName f st y) st 
  -}
-- an example
pairMoveName :: NamedMovable b => b -> (b,b)
pairMoveName ex = (move (Vec 2 3) ex,giveName "test" ex)
 
{-
map f (collapse tr) == collapse (mapTree f tr)  (map-collapse)

LHS = map f (collapse Nil)
= map f []  by (collapse.1) = []  by (map.1)
RHS = collapse (mapTree f Nil)  
= collapse Nil  by (mapTree.1) 
= []  by (collapse.1)

map f (collapse tr1) = collapse (mapTree f tr1)  (hyp.1)
map f (collapse tr2) = collapse (mapTree f tr2)  (hyp.2) 

LHS = map f (collapse (Node x tr1 tr2))
= map f (collapse tr1 ++ [x] ++ collapse tr2)  by (collapse.2) 
= map f (collapse tr1) ++ [f x] ++ map f (collapse tr2)  by (map++)
= collapse (mapTree f tr1) ++ [f x] ++ collapse (mapTree f tr2)  by (hyp.1,hyp.2)

RHS = collapse (mapTree f (Node x tr1 tr2))
= collapse (Node (f x) (mapTree f tr1) (mapTree f tr2))  by (mapTree.2)
= collapse (mapTree f tr1) ++ [f x] ++ collapse (mapTree f tr2)  by (collapse.2)  # 

maybe 2 abs x >= 0  (abs) 
maybe 2 abs Nothing = 2 >= 0
maybe 2 abs (Just y) = abs y >= 0  #

eval (assoc ex) = eval ex  (eval-assoc) 

eval (assoc (Add (Add f1 f2) e3))
= eval (assoc (Add f1 (Add f2 e3)))  by (Assoc.1)

eval (Add f1 (Add f2 e3)) 
= eval (Add (Add f1 f2) e3)  
-}


data Seasons = Spring | Summer | Autumn | Winter deriving (Show,Eq)
data Temp = Cold | Hot deriving (Show,Eq, Ord) 

makeHot True = Hot
makeHot False = Cold
isSummer = (==Summer)

weather Summer = Hot
weather _ = Cold  
{-
newWeather s == weather s

newWeather Summer 
= makeHot True 
= Summer 

newWeather _ 
= makeHot False
= Cold  #

size tr < 2^(depth tr) 

LHS = size NilT = 0  by (size.1)
RHS = 2^(depth NilT)
= 2^0  by (depth.1) = 1 

size tr1 < 2^(depth tr1)  (hyp.1)
size tr2 < 2^(depth tr1)  (hyp.2)
1+max (depth tr1) (depth tr2) = 1+depth tr1  (hyp.3)
LHS = size (Node _ tr1 tr2)
= 1+size tr1+size tr2  by (size.2)

RHS = 2^(depth (Node _ tr1 tr2))
= 2^(1+max (depth tr1) (depth tr2))  by (size.2)
= 2^(1+depth tr1)  by (hyp.3)
= 2*2^(depth tr1)  by (indices'Laws)
= 2^(depth tr1)+2^(depth tr1)  by (*2)
  
-}

my_repeat :: a -> [a] 
my_repeat x = x:my_repeat x

fac 1 = 1
fac n = n*fac (n-1) 





  


  


  