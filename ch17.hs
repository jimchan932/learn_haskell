module Ch17 where 
import Prelude hiding ((&&),minimum,iterate)
import Data.List hiding (minimum,iterate)
import qualified Data.List.Ordered as List 
import Data.Char 

g :: Integer -> Integer -> Integer 
g x y = x+12 

switch :: Integer -> a -> a -> a 
switch n x y 
  | n>0       = x
  | otherwise = y 
  
h :: Num a => a -> t -> a 
h x y = x+x 

pm :: Num a => (a,b) -> a 
pm (x,y) = x+1

addHeads :: [Int] -> [Int] -> Int 
addHeads xs [] = 0
addHeads [] ys = 0 
addHeads (x:xs) (y:ys) = x+y

f :: Int -> Int -> Int 
f m n
  | notNil xs = front xs
  | otherwise = n 
    where 
	xs = [m..n]

front (x:y:zs) = x+y
front [x]      = x

notNil = not.null

(&&) :: Bool -> Bool -> Bool
True &&  x = x 
False && _ = False 

eqList :: Eq a => [a] -> [a] -> Bool  
eqList [] (x:xs) = False 
eqList (x:xs) [] = False 
eqList [] []     = True 
eqList (x:xs) (y:ys) = (x==y) && eqList xs ys 

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]

triangle :: Int -> [(Int,Int)]
triangle n = [(x,y) | x<-[1..n], y<-[1..x]]

pyTriples :: Int -> [(Int,Int,Int)]
pyTriples n = [(x,y,z) | x<-[2..n], y<-[x+1..n], 
                         z<-[y+1..n], x*x+y*y==z*z]

squares n = [m*m | m<-[1..], m*m<=n]

perms1:: Eq a => [a] -> [[a]] 
perms1 [] = [[]]
perms1 xs = [x:ps | x<-xs, ps<-perms1 (xs\\[x])] 

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 (x:xs) = [ps++[x]++qs | rs<-perms2 xs, (ps,qs)<-splits rs]

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = ([],x:xs):[(x:ps,qs) | (ps,qs) <- splits xs]

xs = [x+y | x<-[1..4],y<-[2..4], x>y]

{-
xs
 = [1+y | y<-[2..4], 1>y] ++ [2+y | y<-[2..4], 2>y]
   ++ [3+y | y<-[2..4], 3>y] ++ [4+y | y<-[2..4], 4>y]
 = [3+2]++[4+2]++[4+3]
 = [5,6,7]
 
perm [2] 
 = perm (2:[])
 = [ps++[2]++qs | rs<-perm [], (ps,qs)<-splits rs]
 = [ps++[2]++qs | (ps,qs)<-splits []]
 = [ps++[2]++qs | (ps,qs)<-[([],[])]]
 = [[]++[2]++[]] = [[2]]

perm [2,3]
 = perm (2:[3]) 
 = [ps++[2]++qs | rs<-perm [3], (ps,qs)<-splits rs]
 = [ps++[2]++qs | (ps,qs)<-splits [3]]
 = [[]++[2]++[3]]++[[3]++[2]++[]]
 = [[2,3],[3,2]]

perm [1,2,3]
 = perm (1:[2,3])
 = [ps++[1]++qs | rs<-perm [2,3], (ps,qs)<-splits rs] 
 = [ps++[1]++qs | (ps,qs)<-splits [2,3]]++[ps++[1]++qs | (ps,qs)<-splits [3,2]]
 = [ps++[1]++qs | (ps,qs)<- [([],[2,3]),([2],[3]),([2,3],[])]]++[ps++[1]++qs | (ps,qs)<- [([],[3,2]),([3],[2]),([3,2],[])]]
 = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}

subLists, subSequences :: [a] -> [[a]]

subLists [] = [[]]
subLists (x:xs) = [x:sublist | sublist<- subLists xs]

subSequences [] = [[]]
subSequences ls@(_:xs) = [take n ls | n <- [1..length ls]] ++ subSequences xs

type Vector = [Float]

scalarProduct, scalarProduct2 :: Vector -> Vector -> Float
scalarProduct xs ys = sum [x*y | (x,y)<-zip xs ys]

scalarProduct2 xs ys = sum (zipWith (*) xs ys) 

type Matrix = [Vector]
matrixProduct :: Matrix -> Matrix -> Matrix 
matrixProduct m p = [[scalarProduct r c | c<- columns p] | r<-m]

columns :: Matrix -> Matrix 
columns m = [[r!!n | r<-m] | n<-[0..s]]
  where 
  s = length (head m)-1
  
matrixProduct2 m p = [[scalarProduct r c | c<-transpose p] |r<-m]

refuteRule :: Eq a =>  [[a]] -> [a]
refuteRule = map head. filter (/=[])

refutingPattern :: [[a]] -> [a]
refutingPattern ls = [x | (x:xs)<-ls]

sum4thPowers n = sum (map (^4) [1..n]) 
 
ins :: Ord a => a -> [a] -> [a] 
ins a [] = [a] 
ins a (x:xs) 
  | a<=x = a:x:xs 
  | True = x:ins a xs 

insSort :: Ord a => [a] -> [a]  
insSort = foldr ins [] 

minimum :: Ord a => [a] -> a 
minimum = head . insSort 

infixr 5 >*>

type Parse a b = [a] -> [(b,[a])]

none :: Parse a b 
none _ = []

succeed :: b -> Parse a b 
succeed val inp = [(val,inp)] 

token :: Eq a => a -> Parse a a 
token t = spot (==t) 

spot :: (a -> Bool) -> Parse a a
spot p (x:xs) 
  | p x       = [(x,xs)]
  | otherwise = []
spot p [] = []   

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp 
 
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c) 
(>*>) p1 p2 inp = [((y,z),rem2) | (y,rem1)<-p1 inp, (z,rem2)<-p2 rem1]

build :: Parse a b -> (b -> c) -> Parse a c 
build p f inp = [(f x,rem) | (x,rem) <- p inp]

list :: Parse a b -> Parse a [b] 
list p = (succeed []) `alt` 
         ((p >*> list p) `build` (uncurry (:)))

bracket, dig :: ReadS Char 
bracket = token '(' 		 
dig = spot isDigit 			 

neList, optional :: Parse a b -> Parse a [b]

neList p = (p  `build` (:[]))
           `alt`
           ((p >*> neList p) `build` (uncurry (:)))

optional p = (succeed []) 
             `alt`  
             (p  `build` (:[]))

digList = neList dig 

nTimes :: Int -> Parse a b -> Parse a [b] 
nTimes 0 p xs = []
nTimes n p [] = []
nTimes n p xs = alt (p `build` (:[]))
                ((p >*> nTimes (n-1) p)`build` (uncurry (:))) xs

data Expr = Lit Integer | Var Char |
            Op Ops Expr Expr deriving (Eq,Show)

data Ops = Div | Mod | Mul | Add | Sub deriving (Show,Eq)


parser, litParse, opExpParse :: Parse Char Expr
parser = (litParse `alt` varParse `alt` opExpParse)

varParse xs = (spot isVar `build` Var) xs

opExpParse = (token '(' >*>
              parser    >*>
	          spot isOp >*>
	          parser    >*>
	          token ')')
	         `build` makeExpr

makeExpr :: Num a => (Char,(Expr,(Char,(Expr,Char)))) -> Expr 
makeExpr (_,(e1,(op,(e2,_)))) = Op (charToOp op) e1 e2

charToOp ch =
  case ch of 
    '%' -> Mod 
    '/' -> Div 
    '*' -> Mul 
    '+' -> Add 
    '-' -> Sub

isOp = (`elem` "%/*+-")
 
isVar :: Char -> Bool  
isVar x = ('a' <= x) && (x <= 'z')

litParse = (optional (token '~') >*> neList dig) 
           `build` (charListToExpr . uncurry(++))  

charListToExpr ('~':xs) = (Lit . negate . toInteger . listToInt . map digitToInt) xs
charListToExpr xs = (Lit . toInteger . listToInt . map digitToInt) xs

listToInt [] = 0
listToInt (n:ns) = n*(10^length ns) + listToInt ns
   
topLevel :: Parse a b -> b -> [a] -> b
topLevel p defaultVal inp 
  | null results = defaultVal
  | otherwise    = head results 
    where
    results = [found | (found,[]) <- p inp]

type Var = Char 
data Command = Eval Expr | Assign Var Expr | Null | Raise String deriving Show
commandParse :: Parse Char Command
commandParse = alt (parser `build` Eval) assignParse
  where
  assignParse = (varParse >*> token ':' >*>
                (litParse `alt` opExpParse)) `build` (\((Var x),(_,expr)) -> Assign x expr)

commLine = topLevel commandParse Null
listParse :: Parse Char [Integer]
listParse = build (token '[' >*>
                   digList >*>
	           neList (token ',' >*> digList) `build` map snd >*>
             	   token ']') makeList . filter (/=' ') 
	 
makeList :: (Char,([Char],([[Char]],Char))) -> [Integer]
makeList (_,(x,(xs,_))) = map read (x:xs)  

tokenList :: Eq a => [a] -> Parse a [a]
tokenList [x] ys =
  case (token x ys) of
    [(x,ys)] -> [([x],ys)] 
tokenList (x:xs) ys = build (token x >*> tokenList xs) (uncurry (:)) ys

spotWhile :: (a -> Bool) -> Parse a [a]
spotWhile p xs =
  case (spot p xs) of
    [] -> [([],xs)]
    _  -> build (spot p>*>spotWhile p)(uncurry (:)) xs;

ones = 1 : ones

addFirstTwo :: [Integer] -> Integer
addFirstTwo (x:y:zs) = x+y
{-
addFirstTwo ones
 = addFirstTwo (1:ones)
 = addFirstTwo (1:1:ones)
 = 1+1 = 2   -}

from :: Integer -> [Integer]

from n = n:from (n+1)

fromStep :: Integer -> Integer -> [Integer]
fromStep n m = n: fromStep (n+m) m

pythagTriples :: [(Integer,Integer,Integer)]
pythagTriples = [(x,y,z) | z<-[2..], y<-[2..z-1], x<-[2..y-1], x*x+y*y==z*z]

powers :: Integer -> [Integer]
powers n = [n^x | x<-[0..]]

iterate :: (a -> a) -> a -> [a]
iterate f x = x:iterate f (f x)

primes = 2:sieve [3,5..]

sieve (p:xs) = 
 p : sieve [x | x <- xs, x `mod` p > 0]

badTriples = [(x,y,z) | x<-[2..], y<-[x+1..], z<-[y+1..], x*x+y*y==z*z]

memberOrd :: Ord a => [a] -> a -> Bool
memberOrd (x:xs) y
  | x<y       = memberOrd xs y
  | x==y      = True
  | otherwise = False

isPrime = memberOrd primes

fibonacci = fibF [0..]
  where fibF (x:y:zs) = x: fibF (y:x+y:zs)

-- dunno why factors doesn't work for hamming
factors :: Integer -> [Integer]
factors n = [x | x<-[1..n], n `mod` x==0]

hamming1 = [x | x<-[1..], fact<-[2,3,5], mod x fact==0 ]

hamming2 = [x*fact | x<-[1..], fact<-[2,3,5]]
   -- unsorted, but efficient 

{-
length primes = Infinity
            n < Infinity
take n primes < primes 
-}

runningSums, listSums  :: [Integer] -> [Integer]

runningSums [x] = [x]
runningSums (x:y:zs) = x:runningSums [z | z<-(x+y:zs)]

listSums iList = out
  where out = 0: zipWith (+) iList out
  -- listSums = scanl' (+) 0
  
scanl' :: (a -> b -> b) -> b -> [a] -> [b]
scanl' f st list = out
  where
  out = st: zipWith f list out

sorts :: Ord a => [a] -> [[a]]
sorts = scanl' ins []

factorials = scanl (*) 1 [1..]

powers2 = scanl (*) 1 [2,2..]

positiveSums = runningSums. filter (0<)

fibs = 0:scanl (+) 1 fibs

merge :: Ord a => [a] -> [a] -> [a] 
merge (x:xs) (y:ys)
  | x<y = x:merge xs (y:ys)
  | x==y = x:merge xs ys 
  | x>y = y:merge (x:xs) ys
  
powers_2_3 = merge powers2 (scanl (*) 1 [3,3..])

--duplicates is no concern

simpleMerge = List.mergeBy (\x y -> if x<y then LT else GT)
                                 -- does not compare EQ, assuming the list does not have duplicates
				 -- thus more efficient! 


hamming3 = 1: simpleMerge (simpleMerge [2,4..] [3,6..]) [5,10..]

hamming4 = 1: simpleMerge (simpleMerge (multiples 2) (multiples 3)) (multiples 5)
  where
  multiples n = scanl (+) n [n,n..]

undef :: a
undef = undef

fak n = (n+1)*fak n

x = x+1
    -- non terminating, undefined

list1 = 2:3:undef
list2 = undef:[2,3]
list3 = undef:4:undef

{-
reverse (undef++ys) = reverse xs ++ reverse undef  by (reverse++.u)

reverese (undef ++ xs)
 = reverse undef
 = undef

reverse xs ++ reverse undef
= reverse xs ++ undef  

facMap!!0 = (map fac [0..])!!n  by (facMap)
 = fac ([0..]!!0)  by (map.!!)
 = fac 0 = 1

facMap!!n = facs!!n  (facMap.!!)
facs!!0
 = (1:zipWith (*) [1..] facs)!!0  by (facs)
 = 1 

facMap!!(n-1) = facs!!(n-1)  (ind)

facMap!!n = (map fac [0..])!!n  by (facMap.1)
 = fac ([0..]!!n)  by (map.!!)
 = fac n  by def of [0..],!!
 = n*fac (n-1)  by (fac.2)
 = n* (facMap!!(n-1))

facs!!n = (1:zipWith (*) [1..] facs)!!n  by facs
  = (zipWith (*) [1..] facs)!!(n-1)  by def of !!
  = ([1..]!!(n-1))*(facs!!(n-1))  by (zipWith.!!)
  = n*facs!!(n-1)  by def of [1..],!!
  = n*(facMap!!(n-1)  by (hyp)

take n (map f xs) = map f (take n xs)  (take-map)
take n (zipWith f xs ys) = zipWith f (take n xs) (take n ys) (take-zipWith)

take n facMap
= take n (map fac [0..])  by (facMap)
= map fac (take n [0..])  by (take-map)
= map fac [0..n-1]        by (prop_take)

take (n+1) facMap
= take (n+1) (map fac [0..])  by (facMap)
= map fac (take (n+1) [0..])  by (take-map)
= map fac [0..n]              by (prop_take) 

take n facs
= take n (1:zipWith (*) [1..] facs)  by (facs)
= 1: take (n-1) (zipWith (*) [1..] facs)  by (take.3)
= 1: zipWith (*) (take (n-1) [1..]) (take (n-1) facs)   by (take-zipWith)
= 1: zipWith (*) [1..n-1] (take (n-1) facs)  by def of [1..], take

take (n+1) facs
= = take (n+1) (1:zipWith (*) [1..] facs)  by (facs)
= 1: take n (zipWith (*) [1..] facs)  by (take.3)
= 1: zipWith (*) (take n [1..]) (take n facs)   by (take-zipWith)
= 1: zipWith (*) [1..n] (take n facs)  by def of [1..], take

(map f xs)!!n = f (xs!!n)  (map.!!)

(map f (x:xs))!!0
= (f x: map f xs)!!0   by (map.2)
= f x  by (!!.1)

f ((x:xs)!!0)
= f x   by (!!.1)

(map f (x:xs)!!n
= (f x:map f xs)!!n  by (map.2)
= (map f xs)!!(n-1)
= f (xs!!(n-1))  by (hyp)

f ((x:xs)!!n)
= f (xs!!(n-1))  by (!!.2)

(zipWith f xs ys)!!n = f (xs!!n) (ys!!n)  (zipWith.!!)

zipWith f (x;xs) (y:ys)
= f x y: zipWith xs ys
= f x y  by (!!.1) 

-}

facs = 1: zipWith (*) [1..] facs

facMap = map fac [0..]

prop_take(n) = take n [0..] == [0..n-1] 

fac 0 = 1
fac n = n*fac(n-1)
