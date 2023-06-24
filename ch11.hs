module Ch11 where
import Prelude hiding (uncurry, curry, flip,(.),(++))
import Test.QuickCheck
import Pictures (horse, flipV, flipH, p)

rotate =  flipH >.> flipV 

(a.b) x = a (b x)
(>.>) :: (a -> b ) -> (b -> c) -> (a -> c)
x >.> y = y . x

idFun1 f n = (id . f) n

idFun2 f n = (f.id ) n

composeList (x:xs) = composeList xs . x

sqsum = (\(x,y) -> x*x+y*y)

addSq = map sqsum

mapFuns fs x = map (\f -> f x) fs

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\a b -> g (f a) (f b))

sq = (\x -> x^2)

noSpace :: Char -> Bool
noSpace = (\x -> notElem x " \t\n")

total :: (Integer -> Integer) -> (Integer -> Integer)
total f n = sum $ map (\x -> f x) [1..n]

comp2' f g x = g (f x) 

total' n = addAll [1..n]
  where
  addAll l f = sum $ map f l
  
addPositive :: [Integer] -> [Integer]  
addPositive = map (+1) . filter (>(-1))

mapFuns' fs x = map ($x) fs

multiplyUC = (\(x,y) -> x*y)  

curry :: ((a,b) -> c) -> a -> b -> c
curry g x y = g (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

zipPair = uncurry zip

prop_zip xs = uncurry zip (unzip xs) == xs  

uncurryDollar = uncurry ($)

fac n = uncurryDollar (product,[1..n])

addHead = uncurry (:)

composeFun = uncurry (.) 

uncurryTwice = uncurry uncurry

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 = (\f a b c -> f (a,b,c))

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 = (\f (a,b,c) -> f a b c)
   
doubleAll = map (2*)
   
twice f = f.f

add2 x = (twice succ) x

iter :: Integer -> (a -> a) -> (a -> a)
iter n f 
  | n>0       = f . iter (n-1) f
  | otherwise = id
  
double = (2*) 

power2 n = iter n double 1     

add x = (\n -> iter n succ $ x) 

iterFL n f= compFunL $ replicate n f 
compFunL [] = id
compFunL (x:xs) = x. compFunL xs 

addNum n = let addN m = m+n
           in addN		   

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x
  
flippedMap = flip map

puzzle = (.) (.)

getEvens = filter (\n -> mod n 2==0)

getWord = getUntil (`elem` whitespace) 
 
getUntil p [] = [] 
getUntil p (x:xs)
  | p x       = []
  | otherwise = x: getUntil p xs
  
whitespace = " \n\t"    
dropUntil p [] = []
dropUntil p (x:xs)
  | p x = (x:xs)
  | otherwise = dropUntil p xs

dropWord = dropUntil (`elem` whitespace) 
dropSpace = dropUntil (`notElem` whitespace)

data Person = Person String Int deriving (Show,Eq)
data Lovers = Lovers (String,String) deriving (Show,Eq)

{- 
iter 2 f
= f.iter 1 f  by (iter.1)
= f.(f.iter 0 f)  by (iter.1)
= f.(f.id)  by (iter.2)
= f.f  by (compId)
= twice f  by (twice)

(f.id) x (compId) 
= f (id x) 
= f x  

(f.(g.h)) x (comp)
= f (g.h) x
= f (g (h x)) 
= (f.g.h) x

(id.f) x (compId)
= id (f x)
= f x

flip (flip f) x y 
= flip f y x  by (flip)
= f x y  by (flip)
= (id.f) x y  by (compId)

iter 0 id = id (base)
iter (n-1) id = id (hyp)
iter (n+1) id
= id.iter n id by (iter.1)
= id.(id.iter (n-1) id)  by (iter.1)
= id.iter (n-1) id  by (compId) 
= id  by (hyp)

(signum.signum) (+-n) 
= signum (signum (+-n))
= signum (div n (abs n))
= +-1
signum 0= 0 

(map.3)
LHS = map (f.g) [] = [] (base)  
RHS = (map f.map g) []
= map f (map g [])  by (comp)  
= map f []  by (map.1) = []  by (map.1)  

map (f.g) xs = (map f.map g) xs (hyp)
map (f.g) (x:xs) = (map f.map g) (x:xs) (ind)
LHS = map (f.g) (x:xs)    
= (f.g) x: map (f.g) xs  by (map.2)
= f (g x): (map f.map g) xs  by (hyp)
RHS = (map f.map g) (x:xs)
= map f (map g (x:xs))  by (comp)
= map f (g x: map g xs)  by (map.2)
= f (g x): map f (map g xs)  by (map.2)
= f (g x): (map f.map g) xs  by (comp) #

LHS =(filter p.map f) [] (base) 
= filter p (map f [])  by (comp)
= filter p []  by (map.1) = []  by (filter.1)  
RHS = (map f.filter (p.f)) []
= map f (filter (p.f) [])  by (comp)
= map f []  by (filter.1) = []  by (map.1)  

(filter/map)
(filter p.map f) xs = (map f.filter (p.f)) xs (hyp)
(filter p.map f) (x:xs) = (map f.filter (p.f)) (x:xs) (ind)
LHS = (filter p.map f) (x:xs)
= filter p (f x: map f xs)  by (comp)
= f x: filter p (map f xs)  by (filter.2)
= f x: (filter p.map f) xs  by (comp)
= f x: (map f.filter (p.f)) xs  by (hyp) 
RHS = (map f.filter (p.f)) (x:xs)
= map f (filter (p.f) (x:xs))  by (comp)
= map f (x: filter (p.f) xs)  by (filter.2)
= f x: map f (filter (p.f) xs)  by (map.2)
= f x: (map f.filter (p.f)) xs  by (comp) #

(map/reverse)
LHS = map f (reverse [])  (base)
= map f []  by (reverse.1) = []  by (map.1) 
RHS = reverse (map f [])
= reverse []  by (map.1) = []  by (reverse.1) 
 
map f (reverse xs) = reverse (map f xs) (hyp)
map f (reverse (x:xs)) = reverse (map f (x:xs)) (ind)
LHS = map f (reverse (x:xs)
= map f (reverse xs++[x])  by (reverse.2)
= map f (reverse xs)++map f [x]  by (map++)
= map f (reverse xs)++[f x]  by (map.1),(map.2)
= reverse (map f xs)++[f x]  by (hyp)
RHS = reverse (map f (x:xs))
= reverse (f x: map f xs)  by (map.2)
= reverse (map f xs)++[f x]  by (reverse.2) #

(map++) 
LHS = map f ([]++zs) (base)
= map f zs  by (++.1)   
RHS = map f []++map f zs
= []++map f zs  by (map.1) 
= map f zs  by (++.1)

map f (ys++zs) = map f ys ++ map f zs (hyp)
map f ((y:ys)++zs) = map f (y:ys) ++ map f zs (ind)
LHS = map f ((y:ys)++zs)
= map f (y:(ys++zs))  by (++.2)
= f y:map f (ys++zs)  by (map.2)
RHS = map f (y:ys)++map f zs
= (f y:map f ys)++map f zs  by (map.2) 
= f y:(map f ys++map f zs)  by (++.2)
= f y:map f (ys++zs)  by (hyp) #

* when f is associative
x `f` (y `f` z) = (x `f` y) `f` z (asociative)
* when st is an identity for f
x `f` st = x = st `f` x (identity)

(foldr.3)
LHS = foldr f st ([]++ys) (base)
= foldr f f st ys  by (++.1)
= f st (foldr f st ys)  by (identity)
RHS = f (foldr f st []) (foldr f st ys)
= f st (foldr f st ys)  by (foldr.1)

foldr f st (xs++ys) = f (foldr f st xs) (foldr f st ys) (hyp)
foldr f st ((x:xs)++ys) = f (foldr f st (x:xs)) (foldr f st ys) (ind)
LHS = foldr f st ((x:xs)++ys)
= foldr f st (x:(xs++ys)  by (++.2)
= f x (foldr f st (xs++ys))  by (foldr.2)
= f x (f (foldr st xs) (foldr f st ys))  by (hyp)
RHS =  f (foldr f st (x:xs) (foldr f st ys)
= f (f x (foldr f st xs))  (foldr f st ys)
= f x (f (foldr f st xs) (foldr f st ys))  by (associative) #

concat = foldr (++) []

((0<).(+1)) 0
= 0<1
= 0<=(1-1)
= (0<=) 0 = True
0<=(-1) 
= 0<0
= (0<) (-1+1)
((0<).(+1)) (-1) = False

LHS = filter p (filter q []) (base) 
= filter p []  by (filter.1) = []  by (filter.1)
RHS = filter (p&&&q) [] = []  by (filter.1)

filter p (filter q xs) = filter (p&&&q) xs (hyp)
filter p (filter q (x:xs)) = filter (p&&&q) (x:xs) (ind)
LHS = filter p (filter q (x:xs))
= filter p (x:filter q xs)  by (filter.2)
= x:filter p (filter q xs)  by (filter.2)
= x:filter (p&&&q) xs  by (hyp)
RHS = filter (p&&&q) (x:xs)  
= x:filter (p&&&q) xs  by (filter.2) #
-}

(++) :: [a] -> [a] -> [a]
[] ++ys      = ys
(x:xs) ++ ys = x:(xs++ys)


