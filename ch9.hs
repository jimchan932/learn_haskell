module Ch9 where
import Prelude hiding (length, reverse, unzip, zip, replicate, foldr)
import qualified Prelude
import Test.QuickCheck

import Pictures

foldr f x [] = x
foldr f x (l:ls) = f l (foldr f x ls)

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs  
  -- length [x] = 1
  
fact :: Integer -> Integer
fact n
  | n==0 = 1        -- if  n == 0 = 1 then (4>2) && (fact (-1) == 17) does not give any bool result
  | otherwise = n * fact (n-1)
  
testfac1 = (4>2) || (fact (-1) == 17)
testfac2 = (4>2) && (fact (-1) == 17)

mult :: Integer -> Integer -> Integer
mult a b 
  | (a==0) || (b==0)  = 0   
  | otherwise         = a*b
    -- mult (fact (-1)) 0 = *** Exception: stack overflow
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs
	
prop_sumDoubleAll :: [Integer] -> Bool
prop_sumDoubleAll xs = sum (doubleAll xs) == 2*sum xs
{-
sum (doubleAll []) = 2*sum [] (base)
LHS = sum [] = 0
RHS = 2*0 = 0 
sum (doubleAll (x:xs)) = 2* sum (x:xs) (ind)
LHS = sum (2*x:doubleAll xs) = 2*x + sum (doubleAll xs)
= 2*x + 2* sum xs
RHS = 2*(x+sum xs)
= 2*x + 2*sum xs    by arith

-}
prop_lengthPP :: [a] -> [a] -> Bool
prop_lengthPP xs ys = length (xs++ys) == length xs + length ys
{-
length ([]++ys) = length [] + length ys (base)
LHS = length ys
RHS = 0 + length ys = length ys
length (x:xs ++ ys) = length (x:xs) + length ys (ind)
LHS = length (x:(xs ++ys))
= 1 + length (xs++ys)
RHS = 1+ length xs + length ys
= 1+length (xs++ys) #
-}
reverse [] = [] 
reverse (x:xs) = reverse xs ++ [x]

prop_reversePP :: [Integer] -> [Integer] -> Bool
prop_reversePP xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_PP x = [x] ++ [] == x:[] 

prop_flipVH pic = flipV (flipH pic) == flipH (flipV pic)
-- flipV (flipH []) = flipV [] = [], flipH (flipV []) = flipH [] = [] (base)
-- flipV (flipH (x:xs)) = flipH (flipV (x:xs))         (ind)
-- LHS: flipV (flipH (x:xs)) = flipV (flipH xs ++ [x]) by (flipH.2)  

-- RHS: flipH (flipV (x:xs)) = flipH (flipV xs) ++[x]  by (flipH.2)
-- = flipV (flipH xs ) ++ [x]                          (hyp) #
                                                    
prop_equalVV pic = flipV (flipV pic) == pic

prop_equalHV pic = pic == flipH (flipV pic) 
-- flipH (flipV []) = flipH [] = [] (by flipH.1) , [] = [] (base)				
-- flipH (flipV pic) = pic  (hyp)									
-- flipH (flipV (x:xs)) = (x:xs) (ind)
-- LHS: flipH (flipV (x:xs)) = flipH (flipV xs) ++ [x] by (flipH.2)
-- = xs ++ [x] by (hyp)
-- RHS = (x:xs)      = FALSE #

prop_sum xs ys = sum (xs++ys) == sum xs + sum ys 
-- sum ([] ++ ys) = sum ys, sum [] + sum ys = 0 + sum ys = sum ys (base)
-- sum ((x:xs)++ys) = sum (x:xs) + sum ys (ind)
-- LHS: sum ((x:xs)++ys)
-- = x + sum (xs++ys)  by (sum.2)
-- RHS: sum (x:xs) + sum ys
-- = x + sum ys + sum ys
-- LHS: x + sum (xs++ys) = x + sum xs + sum ys  by (hyp) #

-- xs ++ [] = xs (hyp)
-- [] ++ [] = [] by (++.1)   (base)
-- (x:xs) ++ [] = (x:xs) (ind)
-- LHS: x:(xs ++ [])  by (++.2)
-- = (x:xs)  by (++.2)
-- RHS: (x:xs)   #

{- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs (hyp)
[] ++ (ys ++ zs) = ys ++ zs
, ([] ++ ys) ++ zs = ys ++ zs by (++.1) (base)

(x:xs) ++ (ys ++ zs) = ((x:xs) ++ ys) ++ zs (ind)
LHS: (x:xs) ++ (ys ++ zs) = x:(xs ++ ys ++ zs)  by (++.2)
RHS: ((x:xs) ++ ys) ++ zs
= x:(xs ++ ys) ++ zs       by (++.2)
= x:(xs ++ ys ++ zs)       by (++.2)
 #
-}

{-
sum (reverse []) = sum [] = 0, sum [] = 0 (base) ,by (sum.1) ,by (reverse.1)
sum (reverse l) = sum l (hyp)
sum (reverse (x:xs)) = sum (x:xs) (ind)
LHS: sum (reverse (x:xs)) 
= sum (reverse xs ++ [x]) by (reverse.2) 
= sum (reverse xs) + x    by (sum.2)
= sum xs + x    by (hyp)
RHS: sum (x:xs) 
= x + sum xs     by (sum.2)
 #
-}

{- length (reverse rs) = length rs (hyp)
length (reverse []) = length [] = 0  
length [] = 0  	(base)
length (reverse (x:xs)) = length (x:xs) (ind)
LHS: length (reverse (x:xs))  
= length (reverse xs ++ [x]) by (reverse.2) 
= length (reverse xs ) + 1   by (length.2)
= length xs + 1              by (hyp)
RHS: length (x:xs)
= 1 + length xs              by (length.2)
 #	
-}

averageCal :: [Float] -> Float
averageCal xs = sum xs / fromIntegral (length xs) 

prop_elem z xs ys = elem z (xs ++ ys) == elem z xs || elem z ys
{-

elem z ([] ++ []) = elem z [] || elem z [] (base)
LHS: elem z ([] ++ [])
= elem z []
= False
RHS: elem z [] || elem z []
= False || False
= False
elem z ((x:xs)++ys) = elem z (x:xs) || elem z ys (ind)
LHS: elem z (x:xs) ++ ys)
= elem z (x:(xs ++ ys))
= z==x || elem z (xs++ys) 
elem z (x:xs) || elem z ys
= (z==x || elem z xs) || elem z ys
= z==x || (elem z xs || elem z ys)
= z==x || (elem z (xs++ys)) (ind)
 #
-}

unzip [] = ([],[])
unzip ((x,y):ps)
  = (x:xs,y:ys)
    where
    (xs,ys) = unzip ps

prop_zip ps = zip (fst (unzip ps)) (snd (unzip ps)) == ps
{-
zip (fst (unzip [])) (snd (unzip [])) = [] (base)
zip (fst []) (snd []) = zip [] [] = [] 
zip (fst (unzip (zip xs ys)) (snd (unzip (zip xs ys)) = zip xs ys (ind)
zip (fst (xs,ys)) (snd (xs,ys)) = zip xs ys
 #
-}

zip [] [] = [] 
zip (x:xs) (y:ys) = (x,y) : zip xs ys 

{- 
take n xs ++ drop n xs = xs (hyp)
take n [] ++ drop n [] = [] (base)
take n (x:xs) ++ drop n (x:xs) = (x:xs) (ind)

 #
-}

t _ [] = []
t n xs  | n<=0 = []
t n (x:xs) = x:t (n-1) xs

d _ [] = []
d n xs  | n<=0 = xs  
d n (x:xs) = d (n-1) xs

prop_list n xs = t n xs ++ d n xs == xs 
{-
take n [] ++ drop n [] = [] ++ [] = [] (base)
take n (x:xs) ++ drop n (x:xs) = (x:xs) (ind)
LHS: take n [] 
= []
RHS: drop n [] 
= []
LHS: take n (x:xs) ++ drop n (x:xs) 
x: (take (n-1) xs ++ drop (n-1) xs) 
x: (xs) = x:xs (hyp)
RHS: xs
 #
-}

shunt :: [a] -> [a] -> [a]
shunt [] ys = ys
shunt (x:xs) ys = shunt xs (x:ys)

rev :: [a] -> [a]
rev xs = shunt xs []

prop_rev xs = rev (rev xs) == xs
{-
shunt (shunt [] zs) [] = shunt zs [] (base)
LHS = shunt zs [] 
shunt (shunt xs zs) [] = shunt zs xs (hyp)
shunt (shunt (x:xs) zs) [] = shunt zs (x:xs) (ind)
LHS = shunt (shunt xs (x:zs)) []
= shunt (x:zs) xs     (hyp)
= shunt zs (x:xs) #
-}

prop_revPP xs ys = rev (xs ++ ys) == rev ys ++ rev xs
{-
shunt [] xs = shunt (shunt xs []) [] (base)

shunt xs (y:ys) =shunt (shunt xs (y:xs)) [] (ind)
LHS = shunt (y:xs) ys (hyp)
= shunt xs (y:ys) #
-}

propL x xs ys = x:(xs++ys) == x:xs ++ ys  

facAux :: Integer -> Integer -> Integer
facAux 0 p = p
facAux n p = facAux (n-1) (n*p)
fac2 n = facAux n 1  

fac :: Integer -> Integer
fac n
  | n==0 = 1        
  | otherwise = n * fac (n-1)
{-
fac 0 = facAux 0 1 (base)
fac 0 = facAux 0 1 = 1
n*fac (n-1) = facAux (n-1) (n*p) (hyp)
(n+1)*fac n = facAux n ((n+1)*1) (ind)

(n+1)*fac n = facAux n ((n+1)*1) (hyp)
= facAux n (n+1) #
-}

square 0 = 0
square n = square (n-1) + 2*n-1
prop_sq x = x*x == square x
{-
0*0 = 0 (base)
square n = square (n-1) +2*n-1
         = (n-1)*(n-1)+2*n-1 (hyp)
         = (n*n+2*n+1)+2*n-1
         = n*n #
-} 
data Nat = Zero | Succ Nat deriving (Show,Eq,Ord)

addNat :: Nat -> Nat -> Nat
addNat Zero     y    = y
addNat x        Zero = x
addNat (Succ x) y    = addNat x (Succ y)

prop_addNat x y = addNat x y == addNat y x
