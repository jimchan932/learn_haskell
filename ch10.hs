module Ch10 where
import Test.QuickCheck
import Prelude hiding (map, filter, zipWith, foldr1, foldr, and, or, concat, unzip,init, last, takeWhile, dropWhile drop)
import Ch5 hiding (returnLoan, books, borrowers, borrowed, numBorrowed, makeLoan)
import qualified Ch5
import Pictures hiding (beside, superimpose, p, invertColour, invert)
import qualified Pictures
map f [] = []
map f (x:xs) = f x : map f xs

doubleAll xs = map (2*) xs

filter :: (a -> Bool) -> [a] -> [a]
filter c [] = []
filter c (x:xs) 
  | c x = x: filter c xs
  | otherwise = filter c xs  -- cannot use quickCheck to test these kinds of props
  
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _ _ = []  

beside = zipWith (++)

lengthNum xs = sum (map (^0) xs)   -- cool!

addUp' ns = filter greaterOne (map addOne ns)
  
greaterOne n = n>1 

addUp ns = map (+1) (filter (>0) ns)  

addOne n = n+1

add2 xs = map addOne (map addOne xs) 

prop_add2 xs = add2 xs == map (+2) xs

less10 ns = filter greaterOne (filter (<10) ns)

filterTwice a b xs = filter a (filter b xs)  -- filter a filtered list

squareAll [] = []
squareAll (x:xs) = x^2: squareAll xs

minFun f xs = minimum (map f xs)

allEqual f n = mapAnd (==f 0) (map f [1..n])

greater0 f n = mapAnd (>0) (map f n)

mapAnd :: (t -> Bool) -> [t] -> Bool
mapAnd c x = and (map c x) 

quickSort [] = []
quickSort (x:xs) = quickSort [a | a<-xs, a<x]++[x]++ quickSort [b | b<-xs, b>x]

isSorted xs = xs == quickSort xs

iter 0 f x = x
iter n f x = f (iter (n-1) f x)

double n = 2*n

doTwice f n = f (f n) 

square n = 2^n

power2 n = iter (n-1) double 2 
   -- double 2 counts as 2*2
   -- thus we need to (n-1) times it
   
foldr1 :: (a -> a -> a) -> [a] -> a  
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)   

foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

concat xs = foldr (++) [] xs

and xs = foldr (&&) True xs

or xs = foldr (||) False xs 

rev xs = foldr snoc [] xs
 
snoc :: a -> [a] -> [a] 
snoc x xs = xs ++ [x] 
 
ins :: Integer -> IList -> IList
ins x [] = x:[]
ins n (x:xs)
  | n<=x = (n:x:xs)
  | otherwise = (x:n:xs)

isort = foldr ins []   
  
type Matrix = [[Integer]]  
addMatrices :: Matrix -> Matrix -> Matrix 
addMatrices ((x:xs):xss) ((y:ys):yss) 
  | equalRows ((x:xs):xss) ((y:ys):yss) = addEach (x:xs) (y:ys): addMatrices xss yss
  | otherwise                            = error "There matrices are not equal" 
addMatrices _ _ = []
 
addEach xs ys =  zipWith (+) xs ys

subtractMatrices :: Matrix -> Matrix -> Matrix 
subtractMatrices ((x:xs):xss) ((y:ys):yss)
  | equalRows ((x:xs):xss) ((y:ys):yss)  = subtractEach (x:xs) (y:ys): subtractMatrices xss yss
  | otherwise                            = error "There matrices are not equal"
subtractMatrices _ _ = []
   
equalRows xs ys = rows xs ys && lengthRows xs ys  
  where
  rows xs ys = (length xs == length ys) 
  lengthRows xs ys = map length xs == map length ys
  
subtractEach xs ys =  zipWith (-) xs ys

sumSquares n = foldr (+) 0 (map square [1..n])  

sumListSquares xs = foldr (+) 0 (map square xs)
	
unzip xs = foldr f ([],[]) xs
  where
  f (x,y) (xs,ys) = (x:xs,y:ys)

addMyIndex l = zip [1..length l] l
times2 n = n*2
addTen = iter 10 addOne

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p (x:xs)
  | p x = (x:xs)
  | otherwise = xs  

returnLoan' :: Database -> Person -> Book -> Database
returnLoan' [] _ _ = [] 
returnLoan' (x:xs) pers bk = returnLoan (filterFirst (/=(pers,bk)) (x:xs)) pers bk

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = reverse (filterFirst p (reverse xs))  

mystery xs = foldr (++) [] (map sing xs)
 -- it does Nothing since map sings xs adds a bracket to each list
sing x = [x]

combineList f1 f2  (x:xs) (y:ys) = f1  x:f2 y:combineList f1 f2 xs ys
combineList  f1 f2  _ _  = [] 

drop _ [] = []
drop n l  | n<=0 = l
drop n (x:xs) = drop (n-1) xs

{-
last [] = error "last: empty list"
last [x] = x
last (x:xs) = last xs
-}
getUntil p [] = [] 
getUntil p (x:xs)
  | p x       = []
  | otherwise = x: getUntil p xs
  
getWord xs = getUntil p xs

p x = elem x whitespace   
  
whitespace = [' ','\t','\n']  

takeWhile p xs = getUntil nP xs
  where
  nP x = not (p x)
  
tWhile p [] = [] 
tWhile p (x:xs) 
  | p x = x: tWhile p xs
  | otherwise = []   

dropUntil p [] = []
dropUntil p (x:xs)
  | p x = xs
  | otherwise = dropUntil p xs
  
  
dropWord xs = dropUntil p xs
    
dropSpace xs = dropWhile p xs

nP x = not (elem x whitespace )  
 
invertColour xs = map invertLine xs 
  where
  invertLine xs = map invert xs
  invert x = if x=='.' then '#' else '.'
  
superimpose x y = zipWith imposeString x y 
   where 
   imposeString = zipWith impose 
  

impose x y = if x == '.' && x==y then '.' else '#'

{-
superimposeLine :: String -> String -> String
superimposeLine a b = [if (y==x)&&(y=='.') then '.' else '#' | (x,y)<-zip a b]
 -- zips the ith element of the 2 pictures and changes its char  
superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 = [superimposeLine x y | (x,y)<-zip p1 p2] 
-}
{-
books [] x = []
books ((a,b):l) x
  | x==a  = b:books l x 
  | otherwise = books l x
-} 

borrowedBook x (a,b) = x==a 
isBorrower x (a,b) = x==b 
 
books :: Database -> Person -> [Book]
books dBase pers =	map snd (personBase dBase pers) 

personBase dBase pers = filter (borrowedBook pers) dBase   

borrowers :: Database -> Person -> [Book]   
borrowers dBase pers = map fst borrowerBase
  where 
  borrowerBase = filter (isBorrower pers) dBase 
  
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase person returnedBook = filter ((person,returnedBook)/=) dBase

numBorrowed :: Database -> Person -> Int
numBorrowed dBase pers = length (books dBase pers) 

makeLoan :: Database -> Person -> Book -> Database 
makeLoan dBase pers bk = (bk,pers):dBase 

type NewBase = [(Person,[Book])]


