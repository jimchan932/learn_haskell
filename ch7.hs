module Ch7 where
import Prelude hiding (head, tail, null, sum, product, and, or, concat, unlines, elem, (++), fst, zip , zip3, reverse,
 					   unzip, take, drop,(:), splitAt, getLine, replicate)
import qualified Prelude
import Test.QuickCheck
import Ch5 (digits, fastFib)
import OfficialTypes
import Data.Char
a:[]   = [a]    -- works well!
(x:xs) = [x]++xs  

head :: [a] -> a
head (x:_) = x 

tail :: [a] -> [a]
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null (_:_) = False

firstDigit :: String -> Char
firstDigit st
  = case (digits st) of
      []    -> '\0'
      (x:_) -> x
	  
addFirst :: IList -> Integer
addFirst (x:xs) = x+1

addTwo :: IList -> Integer
addTwo st
  = case (init st) of 
      (x:y:xs) -> x+y -- adds first 2 nums
      []       -> 0   -- gives 0 if only 1 element
      (x:[])   -> x	  -- gives 1st element for 2 elements
	  
add1st :: IList -> Integer
add1st x = head x +1 
  
add2 :: IList -> Integer
add2 i
  | init i == []  = 0
  | length i == 2 = head i
  | otherwise     = sum (take 2 i)  
  
fstDigit :: String -> Char
fstDigit st 
  | digits st == "" = '\0'
  | otherwise       = head (digits st)

sum :: IList -> Integer
sum [] = 0
sum (x:xs)  = x+ sum xs

prop_sum l = sum l == Prelude.sum l

product :: IList -> Integer
product [] = 1
product (x:xs) = x* product xs

prop_product l = product l == Prelude.product l

and, or :: BList -> Bool

and [] = True     -- The last value should be True to return the bool value of (and)
and (x:xs) = x && and xs

prop_and, prop_or, exOrL :: BList -> Bool
prop_and l = and l == Prelude.and l

or [] = False
or (x:xs) = x || or xs

prop_or l = and l == Prelude.and l

exOrL [] = False 
exOrL (x:xs) = exOr x (exOrL xs)

concat ::  [[a]] -> [a]
concat [] = []
concat (x:xs) = x++concat xs

unlines []     = []
unlines (s:ss) = (s++"\n")++unlines ss 

prop_unlines l = unlines l == Prelude.unlines l

(++) :: [a] -> [a] -> [a]
[] ++ys      = ys
(x:xs) ++ ys = x:(xs++ys)

elem ::Eq a => a -> [a] -> Bool
elem x []     = False
elem x (y:ys) = (x==y) || (elem x ys)

fibList :: IList -> IList 
fibList [] = []
fibList (x:xs) = fastFib x : fibList xs  -- really fast

doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

selectEven [] = []
selectEven (x:xs)
  | even x    = x : selectEven xs
  | otherwise = selectEven xs

iSort [] = []
iSort (x:xs) = ins x (iSort xs)  -- recursive

ins :: Integer -> IList -> IList
ins x [] = x:[]
ins x (y:ys)
  | x<=y      = x:(y:ys)
  | otherwise = y : ins x ys  -- x will go to the position 
                               -- behind the larger num							  
elemNum x l = length (element x l)

element x [] = []
element x (y:ys)
  | x==y      = x:element x ys
  | otherwise = element x ys

prop_elemNum x l = elemNum x l >=0  
  
isOne x [] = True 
isOne x (y:ys) 
  | x==y  = False
  | otherwise = isOne x ys 
  
unique [] = []
unique (x:xs) 
  | isOne x xs = x: unique xs   
  | otherwise  = unique xs

put x [] = [x]
put x (y:ys) = y:put x ys 

reverse [] = []
reverse (x:xs) = put x (reverse xs)

prop_reverse l = reverse l == Prelude.reverse l

sndL [] = []
sndL (x:xs) = snd x : sndL xs

fstL [] = []
fstL (x:xs) = fst x : fstL xs 

getBig x [] = x
getBig x (y:ys)
  | x>y       = getBig x ys 
  | otherwise = getBig y ys



isSorted :: IList -> Bool
isSorted ys = (ys == iSort ys)|| (ys == iSort2 ys)

isAscending x [] = True
isAscending x (y:ys) = x<=y && isAscending y ys
  
isDescending x [] = True
isDescending x (y:ys) = x>=y && isDescending y ys
  
isReallySorted [] = True
isReallySorted (x:xs) = isAscending x xs || isDescending x xs

iSort2 :: IList -> IList
iSort2 [] = []
iSort2 (x:xs) = descendingIns x (iSort2 xs)  -- or simply: iSort2 a = reverse (iSort a)
  where
  descendingIns x [] = [x]
  descendingIns x (y:ys) 
    | x>=y      = x:(y:ys)
	| otherwise = y: descendingIns x ys

prop_iSort :: IList -> Bool
prop_iSort x = isReallySorted (iSort x )&& isReallySorted (iSort2 x)	

insPair x [] = [x] 
insPair x (y:ys) 
  | fst x < fst y                   = x:(y:ys)
  | fst x == fst y && snd x < snd y = x:(y:ys)
  | True                      = y:insPair x ys

pairSort [] = []
pairSort (x:xs) = insPair x (pairSort xs)	-- lexicographic						  
						   
zip (x:xs) (y:ys) = (x,y): zip xs ys
zip _ _ = []
 
prop_zip a b = zip a b == Prelude.zip a b
	
ascendingQSort :: IList -> IList	
ascendingQSort [] = []
ascendingQSort (x:xs) = ascendingQSort [s | s<-xs, s<=x] ++[x]++ ascendingQSort [b | b<-xs, b>x]

descendingQSort :: IList -> IList
descendingQSort [] = []
descendingQSort (x:xs) = descendingQSort [a | a<-xs, a>=x] ++[x]++ descendingQSort [b | b<-xs, b<x]  

prop_qSort :: IList -> Bool
prop_qSort x = isReallySorted (ascendingQSort x) && isReallySorted (descendingQSort x)
 
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z): zip3 xs ys zs
zip3 _ _ _ = []  

fst (x,y) = x

take :: Int -> [a] -> [a]
take n xs   | n<=0  = []
take _  [] = []
take n (x:xs)   = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs     | n <= 0 =  xs   
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs -- compare: x : take (n-1) xs
                          -- drop 2 [3,5,56,3] = drop (2-1) [5,56,3] = [56,3]						  
prop_drop x l = drop x l == Prelude.drop x l 
  
splitAt n l = (take n l, drop n l)
prop_splitAt n x = splitAt n x == Prelude.splitAt n x    

zip' (xs,ys) = zip xs ys

unzip :: [(a,b)] -> ([a],[b]) 
unzip [] = ([],[])
unzip l = (fstL l,sndL l)

prop_zip' l = l == zip' (unzip l)
 
prop_testUnzip z = z == unzip (zip' z)  -- Falsifiable!
                                        -- due to some special weird reasons
zip3' [] [] [] = []
zip3' x y z = zip (zip x y) z   -- wrong zip

extractElems x [] = []
extractElems (x:xs) (y:ys) 
  | x==y = x:extractElems xs ys
  | otherwise = extractElems (x:xs) ys 

isSublist xs ys = xs == extractElems xs ys

extractList x [] = [] 
extractList (x:xs) (y:ys)
  | x==y = take (length (x:xs)) (y:ys)
  | otherwise = extractList (x:xs) ys

    	
isSubsequence xs ys = xs == extractList xs ys   

{-
equalElem x [] = []
equalElem (x:xs) (y:ys) 
  | x==y = True: equalElem xs ys
  | otherwise = False:equalElem (x:xs) ys
isSublist xs ys = length xs == lengthEquals 
  where
  lengthEquals = length (filter (==True) (equalElem xs ys))
-}


  
multipyList :: Integer -> IList -> IList
multipyList _ [] = [] 
multipyList n (x:xs) = n*x:multipyList n xs 

whitespace = ['\n','\t',' ']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | elem x whitespace = []
  | otherwise         = x :getWord xs 
  
dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
  | elem x whitespace = (x:xs)
  | otherwise         = dropWord xs -- accepts string after an element of whitespace
  
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
  | elem x whitespace  = dropSpace xs
  | otherwise          = (x:xs)
  
type Word = String

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = [] 
split st = (getWord st) : split (dropSpace (dropWord st))

type Line = [Word]

getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (w:ws)
  | length w <= len   = w: restOfLine
  | otherwise         = []
  where
  restOfLine = getLine newlen ws   
  newlen = len - (length w +1)  

dropLine :: Int -> [Word] -> Line
dropLine n [] = []
dropLine n (x:xs)
  | n < length x = (x:xs) -- analogy of drop 0 xs
  | otherwise = dropLine newlen xs
    where
    newlen = n - (length x +1)  
testString =concatMap (++" ")  ["aa","bb","cc","dd","ee","ff","gg","hh","ii","jj"] 

splitLines :: [Word] -> [Line]
splitLines [] = [] 
splitLines ws
  = getLine lineLen ws
         : splitLines (dropLine lineLen ws)
lineLen = 80
 
fill :: String -> [Line]
fill = splitLines . splitWords		 

haskellInfo ="Haskell Brooks Curry (September 12, 1900 – September 1, 1982) was an American mathematician and logician. Curry is best known for his work in combinatory logic; Curry is also known for Curry's paradox and the Curry-Howard correspondence. There are three programming languages named after him, Haskell, Brooks and Curry, as well as the concept of currying, a technique used for transforming functions in mathematics and computer science."

joinL :: Line -> String
joinL [] = [] 
joinL [x] = x
joinL (x:xs) = x ++ " "++ joinL xs

joinLines :: [Line] -> String
joinLines [] = []
joinLines (x:xs) = joinL x ++ "\n"++ joinLines xs

replicate :: (Num i, Ord i) => i -> a -> [a]  
replicate n x 
  | n<=0 = []
  | True = x:replicate (n-1) x

justifyL [] = []
justifyL (x:xs) 
  | mod lineLen (length (x:xs))==0 = x ++ justify++ justifyL xs 
  | otherwise                      = justifyL (x:xs) ++ addSpace (mod (lineLen - len) len)  
     where
       justify = addSpace (div (lineLen - len) len)	 
       len = lineLength (x:xs) 
addSpace n  
  | n<=0 = []
  | otherwise = ' ':addSpace (n-1) 
  
lineLength [] = 0
lineLength (x:xs) = length x + lineLength xs
  
printText =  putStrLn . joinLines . fill 

getChars :: Word -> Word
getChars [] = []
getChars (x:xs) 
  | elem x whitespace = getChars xs
  | otherwise         = x: getChars xs

getL :: Word -> Word
getL [] = []
getL (x:xs)
  | x/='\n' =  x: getL xs
  | otherwise = []

dropL :: Word -> Word   
dropL [] = []
dropL (x:xs) 
  | x=='\n' = (x:xs)
  | otherwise = dropL xs
 
dropN [] = []
dropN (x:xs)
  | x=='\n' = dropN xs
  | otherwise = (x:xs)  
  
lineList xs = splitN (dropN xs)   
  
splitN [] = []
splitN ws = getL ws : splitN ( dropN (dropL ws))    
  
wc x = (length $ getChars x, length $ split x, length $ lineList x)   

toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs
  
toData [] = []
toData (x:xs)
  | isAlphaNum x = x:toData xs
  | otherwise = toData xs  
  
isPalin :: String -> Bool
isPalin x = testString == reverse newString
  where
  newString = toUpperString $ toData x
  
{-  
subst :: String -> String -> String -> String  
subst a b [] = []
subst a b st 
  | x==a      = (b:xs)
  | otherwise =(x:subst a b xs)   
  where
  (x:xs) = splitWords st
-}

getSpacedWord [] = []  
getSpacedWord (x:xs)
  | elem x whitespace = [x]
  | otherwise         = x:getSpacedWord xs
  
newSplitWords xs = newSplit (dropSpace xs)

newSplit [] = [] 
newSplit xs
  = getSpacedWord xs : newSplit (dropSpace (dropWord xs))  

dropNoSpace [] = []
dropNoSpace (x:xs)
  | x==' ' = xs
  | otherwise = dropNoSpace xs
  
subst a b [] = []
subst a b st
  | a==getSpacedWord st = b++dropNoSpace st
  | otherwise = getSpacedWord st ++ subst a b (dropNoSpace st)

mList _ [] = [] 
mList n (x:xs) = n*x:mList n xs 

