module Ch4 where
import Test.QuickCheck
import Pictures
import Test.HUnit

-- defining max3 by max2
max3 :: Integer -> Integer -> Integer -> Integer
max3 x z y = x `max` y `max` z

max4 :: Integer -> Integer -> Integer -> Integer -> Integer 
max4 w x y z = max w (max3 x y z)
-- (what if?) function for middleNumber 
between :: Integer -> Integer -> Integer -> Bool
between m n p = ((m>n) && (n>p)) || ((p>n) && (n>m))

middle :: Integer -> Integer -> Integer -> Integer
middle x y z 
  | between y x z  = x
  | between x y z  = y
  | otherwise      = z
  
--  a function that only gives "True" when only One of them is True.
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)
  
-- in counting Groups, Please REFER to "How to count like a SuperComputer(from How to Build a Brain!)"
-- in Designing Counting Functions
-- factorial, permutations
howManyof3Equal :: Integer -> Integer -> Integer -> Integer
howManyof3Equal x y z
  | (x==y) && (y==z)               = 3
  |  (x==y) || (y==z) || (x==z)    = 2
  | otherwise                      = 0

-- A special function
equal :: Integer -> Integer -> Integer -> Bool
equal x y z = (x==y) && (y==z)

howManyof4Equal :: Integer -> Integer -> Integer -> Integer -> Integer
howManyof4Equal w x y z
 | (w==x) && (x==y) && (y==z)                                                          = 4
 | (equal x y z) || (equal w x y)  || (equal w y z) || (equal w x z)                   = 3                                   
 | (x==y) || (y==z) || (x==z) || (w==y) || (w==x) || (w==z)                            = 2                                                                   
 | otherwise                                                                           = 0
 
-- The ultimate function for SVG pictures editing
-- Beautiful
fourPics1 :: Picture -> Picture
fourPics1 pic =
    left `beside` right
      where
        stack p  = p `above` invertColour p
        left     = stack pic
        right    = stack (invertColour (flipV pic))

fourPics2 :: Picture -> Picture
fourPics2 pic =
    top `above` bottom
	  where
	  stac p    = p `beside` (invertColour (flipV p))
	  top       = stac pic
	  bottom    = stac (invertColour pic)
	  


-- Beautiful
triArea :: Float -> Float -> Float -> Float 
triArea a b c
  | possible  = sqrt(s*(s-a)*(s-b)*(s-c))
  | otherwise = error "Not triangle inequality"
   where
     s = (a+b+c)/2
     possible = ((c+b) > a) && ((a+c) > b) && ((a+b) > c) 

sumSquare :: Integer-> Integer -> Integer
sumSquare m n = sqM+sqN
    where
	  sqM = m^2
	  sqN = n^2 
	
-- Specially trick yet simple function
isOdd, isEven :: Int -> Bool
isOdd n
-- n is not equal and not smaller than 0,
  | n<=0      = False
  | otherwise = isEven (n-1)
  
isEven n
  | n<0       = False
  | n==0      = True
  | otherwise = isOdd (n-1)  
-- an example is 
  
maxSq :: Integer -> Integer -> Integer  
maxSq x y 
  | sq x > sq y     = sq x 
  | otherwise     = sq y
      where
        sq x = x^2
	   
max3Occur :: Integer -> Integer -> Integer -> (Integer, Integer)
max3Occur x y z
  | (x>=y) && (x>=z) = (x, (howManyEqual x y z))
  | y>=z             = (y, (howManyEqual x y z))
  | otherwise        = (z, (howManyEqual x y z))
  where 	
    howManyEqual x y z
      | (x==y) && (y==z)               = 3
      |  (x==y) || (y==z) || (x==z)    = 2
      | otherwise                      = 0

-- Defining a type with specific data 	  
data Move = Rock | Paper | Scissors
            deriving (Show,Eq)

-- Using the type "Move"			
beat, lose :: Move -> Move	
beat Rock       = Paper
beat Scissors	= Rock
beat _          = Scissors

lose Rock       = Scissors
lose Paper      = Rock
lose _          = Paper
   
data T = Win | Draw | Lose
              deriving (Show,Eq)

result :: Move -> Move -> T
result x y
  | con1         = Win
  | x==y         = Draw
  | con2         = Lose
      where
	  con1 = lose x==y
	  con2 = beat x==y
    

prop_result :: Move -> Move -> Bool
prop_result x y =
   (result x y==Win) || (result x y==Draw) || (result x y==Lose)

data Season =  Spring | Summer | Autumn | Winter
                      deriving (Show, Eq, Ord)
data Temp = Hot | Cold 
                  deriving(Show, Eq, Ord)





location1 :: Season -> Temp -> String
location1 x y
  | climate1                 = "I assume you are in the UK" 
  | (x==Summer) && (y<Cold)  = "I assume you are in Hawaii"
  | otherwise                = "I don't know"
      where
	    climate1 = ((x==Spring) || (x>Summer)) && (y>Hot)
        
data Month =  January | February | March | April | May | June | July | August | September | October | November | December
             deriving(Show, Eq, Ord)



location2 :: Season -> Month -> Month -> String
location2 z x y
  | period1                                    = "I assume you're in Northern Hemispere"
  |(z==Summer) && (x>May) && (y==September)    = "I assume you're  Northern Hemispere"
  | otherwise                                  = "Cannot Determine"
      where
	    period1 = (z==Winter) && (x>November) && (y==March) 

-- Recursion
{-
In this script, the concept is:	  
$ the starting point, the value of fac at 0
$ there is a way of going from the value fac
at a particular point, fac (n-1), to the value
of fac on the next line, namely fac n
-}
-- An example: fac 4 = fac 3 * 4
-- The other functions uses fac as a model
fac :: Integer -> Integer
fac n
  | n==0       = 1
  | n>0        = fac (n-1) * n
  | otherwise  = error "fac only defined on natural numbers"

rangeProduct :: Int -> Int -> Int
rangeProduct m n 
  | n<m       = 0
  | n==m      = m  -- starting point: m
  | n>=m      = rangeProduct m (n-1) * n
  
power2 :: Int -> Int
power2 n
  | n==0      = 1
  | n>0       = 2 * power2 (n-1)

{- Similar to fac.
sumFac (n-1) becomes 
fac n by recursion
-}


{- **In this function, f will substitute 
fac. When inputting fac by this is by 
the (Integer -> Integer) type 
 -}
 -- it will sum up all the factorials from 1 to n
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n      -- this is the type of f n 
  | n==0     = f 0
  | n>0      = sumFun f (n-1) + f n  -- still using this function 

sumFacs :: Integer -> Integer
sumFacs n = sumFun fac n

regions :: Integer -> Integer
regions n 
  | n==0   = 1   -- a paper has 1 piece
  | n>0    = regions (n-1) + n

calSum :: Int -> Int
calSum n 
  | n==0   = 0
  | n>0    = calSum (n-1) + n
  
add x y = x+y  

multipySum :: (Int -> Int) -> Int -> Int
multipySum s n 
  | n==0  = s 1
  | n>0   = multipySum s (n-1)* s n

-- whatever!
-- My next function to write is to find the fibonacci sequence
	  
whiteSquares, blackSquares :: Int -> Picture

whiteSquares w 
  | w<=1    = white
  | otherwise = white `beside` whiteSquares (w-1)
  -- width of black and white is 6 chars
 {- I suppose these functions work by this:
    let n be the no. inputted in whiteSquares or blackSquares
	let n also be the no. of black or white inputted (they are both related) 
	 n = 1 + (n-1)
----------------------
That means if n is no. of black,
 then no. of black = black = (no.of black - black) 
  ** thus displaying number of blackSquares
-}
	  
blackSquares n 
  | n<=1   = black   
  | otherwise = black `beside` blackSquares (n-1)  
    -- beautiful!
	-- Always THINK RECURSIVLEY!
	   
blackWhite, whiteBlack  :: Int -> Picture
blackWhite n
  | n<=1   = black
  | otherwise = black `beside` whiteBlack (n-1)
whiteBlack b 
  | b<=1  = white
  | otherwise  = white `beside` blackWhite (b-1) 

-- whiteBlack: white `beside` black `beside` white `beside` black ......
-- inter-related concept with blackWhite 



  -- m for height
  -- n for width
  
column :: Picture -> Int -> Picture 
column p n 
  | n<=1  = p 
  | otherwise = p `above` column p (n-1)  
  
testColumn :: IO()
testColumn = printPicture (column horse 3)

fib :: Int -> Int
fib n 
  | n==0  = 0 
  | n==1  = 1
  | n>1   = fib (n-2) + fib (n-1)

fibSeq = [ fib x | x <- [1..12]]

longerFibSeq = fibSeq++[fib x |x<-[13..24]]

power2Cal :: Int -> Int
power2Cal x
  | x==0        = 1
  | even x = 2*(2*power2Cal (x-1))
  | odd x  = (power2 x)^2*2
-- done with creativity!

allEqual :: Integer -> Integer -> Integer -> Bool
allEqual a b c 
  | (a==b)&&(b==c)  = True
  | otherwise       = False

testEq1 = TestCase (assertEqual "for: allEqual 2 2 2" True (allEqual 2 2 2))
testEq2 = TestCase (assertEqual "for: allEqual 2 2 3" False (allEqual 2 2 3))
testEq3 = TestCase (assertEqual "for: allEqual 2 3 2" False (allEqual 2 3 2))
testEq4 = TestCase (assertEqual "for: allEqual 2 3 4" False (allEqual 2 3 4))
testsEq = TestList [testEq1,testEq2,testEq3,testEq4]

allDifferent :: Integer -> Integer ->  Integer -> Bool
allDifferent a b c
  | (a/=b) && (c/=a) = True
  | otherwise        = False
 
testDiff1 = TestCase (assertEqual "for: allDifferent 2 2 2" False (allDifferent 2 2 2))
testDiff2 = TestCase (assertEqual "for: allDifferent 2 3 2" False (allDifferent 2 3 2))
testDiff3 = TestCase (assertEqual "for: allDifferent 2 2 3" False (allDifferent 2 2 3))
testDiff4 = TestCase (assertEqual "for: allDifferent 2 3 4" True  (allDifferent 2 3 4))
testsDiff = TestList [testDiff1,testDiff2,testDiff3,testDiff4]

averageCal :: Float -> Float -> Float -> Float
averageCal a b c = (a+b+c)/3 

howManyAboveAverage :: Float -> Float -> Float -> Float
howManyAboveAverage a b c
  | (a>average&&b>average)&&c>average = 3
  | (a>average && b>average) || (a>average && b>average) = 2
  | b>average && c>average  = 2
  | a<0                     = error "not calculated with negative numbers"
  | (a==average&&b==average)&&c==average = 0
  | otherwise               = 1
    where
	average = (a+b+c)/3
	
testAbove1 = TestCase (assertEqual "for: howManyAboveAverage 4 4 4" 0 (howManyAboveAverage 4 4 4))	
testAbove2 = TestCase (assertEqual "for: howManyAboveAverage 4 4 4" 2 (howManyAboveAverage 7 7 4))
testAbove3 = TestCase (assertEqual "for: howManyAboveAverage 4 4 2" 2 (howManyAboveAverage 4 7 7))
testAbove4 = TestCase (assertEqual "for: howManyAboveAverage 4 4 2" 1 (howManyAboveAverage 4 4 7))
testsAbove = TestList [testAbove1,testAbove2,testAbove3,testAbove4]  -- success!

