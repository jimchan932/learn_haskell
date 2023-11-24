module Ch5 where
import Ch3
import Test.QuickCheck
import Test.HUnit
import Data.Char
import Pictures
import Data.List

testing x
  | x==0 = 1
  | x>0  = 2*testing (x-1)
  
add1Fun :: (Integer -> Integer) -> Integer -> Integer
add1Fun f n = 1+ (f n) -- works!

fibTriple n = (fib (n-1),fib n, fib (n+1))

fib :: Integer -> Integer
fib n
  | n==0   = 0
  | n==1   = 1
  | n>1 = fib (n-1) + fib(n-2)  
{-  
fib 2 = fib (2-1) + fib (2-2)
= fib 1 + fib 0
= 1 + 0  = 1

fib 3 = fib (3-1) + fib (3-2)
= fib 2 + fib 1
= (1 + 0) + 1
= 1+1  = 2     Beautiful!
-}  
fibStep :: (Integer,Integer) -> (Integer,Integer)  
fibStep (u,v) = (v,u+v)  
  
fibPair :: Integer -> (Integer,Integer)
fibPair f 
  | f==0   = (0,1)
  | otherwise = fibStep (fibPair (f-1))

fastFib :: Integer -> Integer
fastFib = fst . fibPair -- the first of fibPair (n), which is fib n

fibTest = fibStep (2,3)

maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs a b 
  | a>b = (a,1)
  | b>a = (b,1)
  | otherwise = (0,0)
  
testMax1 = TestCase (assertEqual "for: maxOccurs 1 1" (0,0) (maxOccurs 1 1))
testMax2 = TestCase (assertEqual "for: maxOccurs 2 1" (2,1) (maxOccurs 2 1))
testMax3 = TestCase (assertEqual "for: maxOccurs 1 2" (2,1) (maxOccurs 1 2))
testsMaxOccur = TestList [testMax1,testMax2,testMax3]

between :: Integer -> Integer -> Integer -> Bool
between m n p = ((m>=n) && (n>=p)) || ((p>=n) && (n>=m))

middle :: Integer -> Integer -> Integer -> Integer
middle x y z 
  | between y x z  = x
  | between x y z  = y
  | otherwise      = z
  
orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (a,b,c) = (minThree a b c, middle a b c, maxThree a b c)

testTriple1 = TestCase (assertEqual "for: orderTriple (1,2,3)" (1,2,3) (orderTriple (1,2,3)))  
testTriple2 = TestCase (assertEqual "for: orderTriple (2,2,1)" (1,2,2) (orderTriple (2,2,1)))  
testTriple3 = TestCase (assertEqual "for: orderTriple (2,1,2)" (1,2,2) (orderTriple (2,1,2)))
testTriple4 = TestCase (assertEqual "for: orderTriple (2,2,2)" (2,2,2) (orderTriple (2,2,2))) 
testsTriple = TestList [testTriple1,testTriple2,testTriple3,testTriple4]  -- success!

type Age = Int
type Name = String
type People1 = (Name,Age)
data People2 = Person Name Age deriving (Show,Eq)
  -- Person is a constructor
  -- The binary constructor takes arguments
  
showPerson1 :: People1 -> String
showPerson1 (st,n) = st ++ " -- " ++ show n

showPerson2 :: People2 -> String
showPerson2 (Person st n) = st ++ " -- " ++ show n

type Product = String
type Price = Integer
data ShopItem = ShopItem Product Price deriving (Show,Eq)

basketProduct :: ShopItem  -> String
basketProduct (ShopItem i p) = "Product: "++i++showPrice
  where
    showPrice = "  Price: "++show p 


data Address = HouseName String | HouseNum Int deriving (Show,Eq)

houseAddress :: Address -> String
houseAddress (HouseName n) = "Home address: "++n++" House"
houseAddress (HouseNum i) 
  | i `elem` hNums 1 = "Home address: "++show i++"st House"
  | i `elem` hNums 2 = "Home address: "++show i++"nd House"
  | i `elem` hNums 3 = "Home address: "++show i++"rd House"
  | otherwise  = "Home address: "++show i++"th House" 
  where
  hNums n = map (+n) ([0]++[20,30..1000])
  -- worked!
  
type IList = [Integer]
type SList = [String]
type BList = [Bool]
type IPair = (Integer,Integer)
type ITriple = (Integer,Integer,Integer)
type IQuadriple = (Integer,Integer,Integer,Integer)
type SPair = (String,String)
type STriple = (String,String,String)
type SQuadriple = (String,String,String,String)

testL :: IList -> Integer
testL n = sum n 
 
doubleEvenL x = [2*n | n<-x, even n]
doubleEvenL2 x = [2*n | n<-x, even n, n>3]

doubleEven x = [2*y | y<-x, even y]

addPairs pairList = [m+n | (m,n) <- pairList]
-- must use list because [ ] is for list

addOrdPairs :: [IPair] -> [Integer]
addOrdPairs pairList = [m+n | (m,n)<-pairList, m<n]

digits :: String -> String
digits st = [ch | ch<-st, isDigit ch] -- returns digits ONLY
-- beautiful

evenList :: Integer -> IList
evenList n = [x | x<-[1..n] , even x]

allEven, allOdd :: IList -> Bool
allEven xs = xs == [x | x<-xs, even x] -- only returns even numbers.
                                         -- no numbers will return True ????
allOdd xs = [] == [x | x<-xs, even x]
  -- allEven [1,3,5,79,255] = []

isEven :: IList ->  BList
isEven n = [even x | x<-n]  

lEven :: IList -> BList 
lEven m = map even m

triArea :: Float -> Float -> Float -> Float 
triArea a b c
  | possible  = sqrt(s*(s-a)*(s-b)*(s-c))
  | otherwise = error "Not triangle inequality"
   where
     s = (a+b+c)/2
     possible = ((c+b) > a) && ((a+c) > b) && ((a+b) > c) 
  	 
data Shape = Circle Float | Rectangle Float Float |
			 Triangle Float Float Float
             deriving (Show,Eq,Ord)

isRound :: Shape -> Bool
isRound (Circle _)  = True
isRound (Rectangle _ _ ) = False
isRound (Triangle _ _ _) = False

data Lines = Diameter deriving (Show,Eq)
bisectCircle :: Lines -> Bool
bisectCircle Diameter = True  

area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle h w) = h*w 
area (Triangle a b c) = triArea a b c

regular :: Shape -> Bool
regular (Rectangle a b) = if a==b then True else False
regular (Circle _)  = True
regular (Triangle a b c) = if (a==b)&&(b==c) then True else False
	 
perimeter :: Shape -> Float
perimeter (Rectangle h w) = (h+w)*2
perimeter (Circle r) = 2*r*pi
perimeter (Triangle a b c) =  sum [a,b,c] 

equalShape :: Shape -> Shape -> Bool 
equalShape (Circle a) (Circle b) 
  | signum a== (-1) && signum b== (-1) = True
  | a==b                         = True
  | otherwise                    = False

totalRadii :: [Shape] -> Float
totalRadii shapes = sum [r | Circle r<-shapes]
-- sum of Radii 

doubleAll :: IList -> IList
doubleAll n = [2*x | x<-n]  

listAz :: String
listAz = ['A'..'Z']++['a'..'z']

zipAz :: [(Int, Char)]
zipAz = zip (map fromEnum listAz) listAz

charList :: [Char]
charList = [toEnum x | x <- [1..127]]

capitalize :: String -> String 
capitalize s = [if c>'Z' then toEnum (fromEnum c-32) else c | c<-s] 
-- beautiful!  since this is a list comprehension, all lower-case letters will be converted to Upper-case 

capitalizeLetters :: String -> String 
capitalizeLetters s = [if c>'Z' then toEnum (fromEnum c-32) else c | c<-s ,elem c listAz] 
-- VERY beautiful!  

divisors :: Integer -> IList
divisors n = [a | a<-[1..n], mod n a == 0]
  
checkPrime :: Integer -> (Bool,String)
checkPrime n 
  | divisors n == [1,n] = (True,"divisors: "++showDiv n)
  | otherwise  = (False,"divisors: "++showDiv n) -- Perfect
  where 
  showDiv n = show (divisors n)

isPrime :: Integer -> Bool
isPrime n 
  | divisors n == [1,n] = True
  | otherwise  = False 
  
twoPrimes (a,b) = isPrime a&&isPrime b 

twinPrimes :: Integer -> [(Integer,Integer)]  
twinPrimes n =  [(x,y) | x<-[1..n], y<-[1..n],  y-2==x && twoPrimes (x,y),y>x ]  

listPrimes :: Integer -> IList
listPrimes n = [x | x<-[1..n], isPrime x] 

manyPrimes :: IList
manyPrimes = listPrimes 7000

bigPrimes :: IList
bigPrimes = [x | x<-[10000000..12000000], isPrime x]

duplicate :: String -> Int -> String
duplicate s n 
  | n==1  = s
  | n>1   = s++duplicate s (n-1)  
  | otherwise  = " "

fibTable :: Integer -> String
fibTable n = "n"++"\t    "++"fib n"++fibL n
  where 
    fibL n = unwords ["\n"++show x++"\t\t"++show (fib x)| x<-[1..n]]
      -- beautiful some way!

sameParityList :: Integer -> IList
sameParityList n = if odd n then [1,3..n] else [2,4..n] 

  
pythTriples :: Integer ->[ITriple]
pythTriples n = [(a,b,c) | a<-[1..n],b<-[1..n],c<-[1..n] ,(c>b)&&(b>a) ,a^2+b^2==c^2]

numLogic :: Integer -> Bool
numLogic n = if n<=0 then False else True  -- not really understanding for this 
  -- not standard in Haskell
  
threeDs :: Integer -> ITriple
threeDs n = (n,n^2,n^3)

type TestSType = String -> String 

testS :: TestSType
testS "Try" = "test Successful!" 

data Elem = Elem Integer deriving (Show,Eq) 

in3Brackets :: [[[Elem]]] -> [Elem]
in3Brackets xsss = [xs | [[xs]] <-xsss]  
  -- returns [Elem 1,Elem 100] in [[[Elem 1]],[[Elem 100]]]
  
sings :: [IList] -> IList
sings xss = [x | [x] <-xss]  -- returns [1] in [[1]]

matches :: Integer -> IList -> IList
matches n l = [x | x<-l, n==x]

pushLeft :: String -> String 
pushLeft s = if length s<linelength then s++(duplicate " " (linelength - length s))  else s
  
linelength = 12  

pushRight :: String -> String 
pushRight s = if length s<linelength then (duplicate " " (linelength - length s))++s  else s

onSeperateLines :: SList -> String
onSeperateLines s = concat [ if length x<78 then x++(duplicate " " (80-length x)) else x | x<-s] 

testSeperate :: String
testSeperate = onSeperateLines ["Hello World!","Testing, testing","You are successful"]

type Database = [(Person,Book)] 	 
type Person = String
type Book   = String

exampleBase :: Database
exampleBase = [("Jim","Haskell: the craft of functional programming"),
               ("Jim","JavaScript (Third Edition)"),("Ethan","Programming in Objective-C (5th Edition)"),
			   ("Vincent","BASIC OF MATLAB and Beyond"),("Tom","Haskell: the craft of functional programming"),
			   ("Tom","Diary of a wimpy kid"),("Elsa","DORK diaries"),("Elsa","Diary of a wimpy kid"),
			   ("Vincent","Harry Potter and the DEATHLY HALLOWS")]
books :: Database -> Person -> [String]
books dBase findPerson = [book | (person, book)<-dBase, person==findPerson]
 -- this is a model of the lookup function 

numBorrowed :: Database -> Person -> Int
numBorrowed dBase person = length (books dBase person)

borrowed :: Database -> Book -> Bool
borrowed dBase bk = elem bk [book | (person,book)<-dBase]

borrowers :: Database -> Book -> [Person]
borrowers dBase bk = [person | (person,book)<-dBase, bk==book]
  
makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase person borrowedBook = [(person,borrowedBook)] ++ dBase


returnLoan ::  Database -> Person -> Book -> Database
returnLoan dBase person returnedBook = [pair | pair <- dBase, pair /= (person,returnedBook)] 

-- remove the person and the book from database

data Loan = Loan Person Book deriving (Show,Eq)  
-- always write deriving (Show,Eq) ! ***  

exampleBase2 :: [Loan]
exampleBase2 = [Loan p b |  (p,b)<-exampleBase]
  
prop_db1, prop_db2 :: Database -> Person -> Book -> Bool
prop_db1 dBase pers bk =
    elem bk loanedAfterLoan == True
         where
		 afterLoan = makeLoan dBase pers bk 
		 loanedAfterLoan = books afterLoan pers

prop_db2 dBase pers bk =
    elem bk loanedAfterReturn == False
         where
		 afterReturn = returnLoan dBase pers bk
		 loanedAfterReturn = books dBase pers

newBase = [(p,books exampleBase p) | (p,book)<-exampleBase]

  
makeSingle e l = [x | x<-l, e==x]


radius :: [Float] -> [Shape]
radius r = [Circle n | n<-r] 

data Parity = Odd | Even deriving (Show,Eq)
parityList :: IList -> [Parity]
parityList x = [if even y then Even else Odd | y<-x] 


