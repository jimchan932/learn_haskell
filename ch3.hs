module Ch3 where
import Test.QuickCheck 
import Text.Parsec

--  a function that only gives "True" when only One of them is True.
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

-- same as exOr 
exOr1 :: Bool -> Bool -> Bool
exOr1 x y = (x || y) /= (x && y)

prop_exOrs :: Bool -> Bool -> Bool
prop_exOrs x y = 
    exOr x y == exOr1 x y

-- same as prop_exOrs
prop_exOr2 :: Bool -> Bool -> Bool
prop_exOr2 x y = 
    exOr x y == (x /= y)

-- a fucntion that is SAME as " not "
myNot :: Bool -> Bool
myNot True = False
myNot False = True

prop_myNot :: Bool -> Bool
prop_myNot x =
    not x == myNot x

-- a  SPECIAL Function to understand!
mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m==n) && (n==p))

-- the function tests whether (x=y), (y=z) but NOT (x=z)!
threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual x y z = (x==y) && (y==z)  

threeDifferent1 :: Integer -> Integer -> Integer -> Bool
threeDifferent1 m n p = (m/=n) && (n/=p) && (m/=p)
 
 -- Similar to threeEqual
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual p q r s = (p==q) && (q==r) && (r==s) 


max1 :: Integer -> Integer -> Integer
max1 x y = if x > y then x else y

max2 :: Integer -> Integer -> Integer
max2 x y 
  | x > y    = x
  | otherwise = y
  
prop_compareMax :: Integer -> Integer -> Bool
prop_compareMax x y = max2 x y == max1 x y

prop_max1, prop_max2 :: Integer -> Integer -> Bool

prop_max1 x y =
  x < max1 x y && y < max1 x y

prop_max2 x y =
  x == max1 x y || y == max1 x y
  
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z 
  | x >= y && x >= z    = x
  | y >= z              = y
  | otherwise           = z
  
max3 :: Integer -> Integer -> Integer -> Integer
max3 x z y = (max1 x y) `max1` z 
  
min :: Int -> Int -> Int
min x y
  | x < y    = x
  | otherwise = y
  
minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z
  | x <= y && x <= z   = x
  | y <= z             = y
  | otherwise          = z
  
-- **fromEnum and toEnum are STANDARD Functions(NO Need to Declare)

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch-32)


charSubInt :: Int
charSubInt = fromEnum '8' - 8

charToDigit :: Char -> Int
charToDigit ch
  | ('0' <= ch) && (ch <= '9') = (fromEnum ch - charSubInt)
  | otherwise                  = 0

-- Example strings
str1, str2, str3, str4, str5 :: String
str1 = "My name: Jim"
str2 = ""
-- '\t' '\n' are not Strings
str3 = "\99a\177"
str4 = "Special Chars: \t Hello! \n Goodbye!"
str5 = "1\t2\t3"

--IO () type might be for functions that are using prelude( with standard functions )

pstr1, pstr2, pstr3, pstr4, pstr5 :: IO ()
pstr1 = putStr str1
pstr2 = putStr str2
pstr3 = putStr str3
pstr4 = putStr str4
pstr5 = putStr str5


romanDigit :: Char -> String
romanDigit '1' = "I" 
romanDigit '2' = "II"
romanDigit '3' = "III"
romanDigit '4' = "IV"
romanDigit '5' = "V"
romanDigit '6' = "VI"
romanDigit '7' = "VII"
romanDigit '8' = "VIII"
romanDigit '9' = "IX"

cal1 :: Float
cal1 = sin (pi/4) * sqrt 2

-- Functions to caculate Volumes and Areas
circleArea :: Double -> Double
circleArea r = r^2*pi 

sphereArea :: Double -> Double 
sphereArea r = 4*pi*r^2

sphereVolume :: Double -> Double
sphereVolume r = 4/3*pi*r^3

-- My proof and formulaes for Trigonometric identities
triIdentity1 :: Double -> Double
triIdentity1 x = (sin x)^2 + (cos x)^2

triIdentity2 :: Double -> Double
triIdentity2 x = sin x/cos x

prop_triIdentity1 :: Double -> Bool
prop_triIdentity1 x = triIdentity1 x == 1.0

prop_triIdentity2 :: Double -> Bool
prop_triIdentity2 x = triIdentity2 x == tan x
  
funny x = x+x


peculiar y = y+1
