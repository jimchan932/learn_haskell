module Ch12 where
import Prelude hiding (cycle,succ, replicate, lines)
import Data.Time
import System.Locale
import System.IO.Unsafe
import System.IO
import Data.Char
import System.Console.ANSI

f x = 2*x 

(>.>) :: (a -> b) -> (b -> c) -> a -> c
f >.> g = g.f 

type Picture = [[Char]]

horse :: Picture 
horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]

flipH :: Picture -> Picture			 
flipH = reverse  ChangeType 

flipV :: Picture -> Picture		 
flipV = map reverse 

above :: Picture -> Picture	-> Picture
above = (++)

beside :: Picture -> Picture -> Picture	 
beside = zipWith (++)

invertColour :: Picture -> Picture
invertColour = map (map invertChar)
  where
  invertChar = \c -> if c=='.' then '#' else '.'
  
printPicture = putStrLn.unlines

combineChar :: Char -> Char -> Char
combineChar = \x y -> if (x == '.' && y == '.') 
                      then '.' else '#'
		  
superimpose :: Picture -> Picture -> Picture	 
superimpose = zipWith $ zipWith combineChar
  
makePicture :: Int -> Int -> [(Int, Int)] -> Picture
makePicture width height dots
    = map (\h -> map (drawDot h) [0..width-1]) [0..height-1]
    where
    isBlack x y     = elem (x, y) dots
    drawDot x y
      | isBlack x y = '#'
      | otherwise   = '.'

type Position = (Int,Int)
type Image = (Picture,Position)

succDimen l x = map (\n -> x) [0..l-1]
makeImage pic (x,y) = left `beside` pic `above` bottom
    where
    makeSpace x y = succDimen y $ succDimen x ' '
    left = makeSpace x (length pic) 
    bottom = makeSpace x y
	
printImage :: Image -> IO()
printImage = printPicture.fst 
	  
data Move = Rock | Paper | Scissors deriving (Show,Eq,Ord)
type Strategy = [Move] -> Move
randomInt :: Integer -> IO Integer
randomInt n =
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)
randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt
 
rock,paper,scissors,cycle :: Strategy
rock _ = Rock
paper _ = Paper
scissors _ = Scissors
cycle moves
  = case (length moves) `rem` 3 of
      0 -> Rock
      1 -> Paper
      2 -> Scissors
alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2 moves =
    case length moves `rem` 2 of
      1 -> str1 moves
      0 -> str2 moves
 
sToss :: Strategy -> Strategy -> Strategy
sToss str1 str2 moves = map ($moves) [str1,str2]!!(fromIntegral $ randInt 2)
 
sTossList :: [Strategy] -> Strategy
sTossList [] = rock
sTossList xs = xs!!(fromIntegral $ randInt (toInteger $length xs))
 
beatStrategy :: Strategy -> Strategy
beatStrategy opponent moves = beat (opponent moves)
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock
 
type RegExp = String -> Bool
 
epsilon :: RegExp
epsilon = (=="")
 
char  :: Char ->  RegExp
char ch = (==[ch])
  
splits xs = [splitAt n xs | n <- [1..length xs]]
 
(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 =
  \x -> e1 x || e2 x
 
(<*>) :: RegExp -> RegExp -> RegExp
e1 <*> e2 =
  \x -> or [e1 y && e2 z | (y,z) <- splits x] 

star :: RegExp -> RegExp
star p = epsilon ||| (p <*> star p)

a,b :: RegExp
a = char 'a'
b = char 'b'

ab = star ((a ||| b) <*> (a ||| b))

option :: RegExp -> RegExp  
option p = epsilon ||| (p <*> (not.option p))    

type Natural a = (a->a) -> a -> a

zero, one, two :: Natural a
zero f = id
one f = f
two  f = f.f
three f = two f.f

int :: Natural Int -> Int
int n = n (+1) 0

succ :: Natural Int -> Natural Int
succ n f = f.n f 

plus,times :: Natural Int -> Natural Int -> Natural Int  
plus m n = \f -> (m f. n f) 

times = \a b -> a.b

toPic :: Pixel -> Picture 
toPic x = replicate 10 (replicate 10 x)

type Pixel = Char
type FloatPos = (Float,Float) 

dimenPic :: (Int,Int) -> Pixel -> Picture 
dimenPic (x,y) = replicate y.replicate x

pairFun f (x,y) = (f x, f y)  

floatPic :: FloatPos -> Pixel -> Picture
floatPic = (dimenPic).pairFun (round.(*10))

positionedPic :: (Position,Position) -> Pixel -> Picture
positionedPic ((l,b),(r,u)) x = left `beside` (pic `above` bottom)
    where
	  left = dimenPic (l,u) ' '
	  bottom = dimenPic (r,b) ' '
	  pic = dimenPic (r-l,u-b) x

bitAbove f1 (a,b) m f2 = ((f1 (a,b) m)`above`). f2   	  

replicate 0 x = []
replicate n x = x:replicate (n-1) x

main = do
    setCursorPosition 5 0
    setTitle "ANSI Terminal Short Example"
    
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Red
           ]
    putStr "Hello"
    
    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    putStrLn "World!" 

	
getInt = do x <- getLine
            return (read x :: Int)
colouredPic = do
    setSGR [SetColor Foreground Vivid Green] 
    putStr "Input width of the pic "
    x <- getInt
    if x>80
	then return () 
    else do putStrLn "Input heigth of the pic "
            y <- getInt  
            let pic = dimenPic (x,y) '#' 
            printPicture pic	
type Doc = String
type Line = String
type Word = String

whitespace = " \t\n:;.,\"\'!?()-"
getWord = takeWhile (`notElem` whitespace)
dropWord = dropWhile (`notElem` whitespace)
dropSpace = dropWhile (`elem` whitespace) 
splitWords = split.dropSpace
split [] = [] 
split st = getWord st : split (dropSpace (dropWord st))

getL = takeWhile (/='\n') 
dropL = dropWhile (/='\n')
dropN = dropWhile (=='\n') 

lines :: Doc -> [Line]
lines [] = []
lines xs = getL xs:lines (dropN (dropL xs))

numLines :: [Line] -> [(Int,Line)]
numLines linels = zip [1..length linels] linels

listWords :: (Int,Line) -> [(Int,Word)]
listWords (num,line) = [(num,word) | word<-splitWords line]

allListWords :: [(Int,Line)] -> [(Int,Word)]
allListWords = concat. map listWords

orderPair :: (Int,Word) -> (Int,Word) -> Bool
orderPair (n1,w1) (n2,w2) = w1<w2 || (w1==w2 && n1<=n2)

sortL :: [(Int,Word)] -> [(Int,Word)]
sortL [] = []
sortL (p:ps) = sortL smaller ++ [p] ++ sortL larger
  where
  smaller = [q | q<-ps, orderPair q p]
  larger  = [q | q<-ps, orderPair p q]
  
makeList :: [(Int,Word)] -> [([Int],Word)]  
makeList = map (\(n,st) ->([n],st)) 
amalgamate :: [([Int],Word)] -> [([Int],Word)]    
amalgamate [] = []  
amalgamate [p] = [p]  
amalgamate ((l1,w1):(l2,w2):rest) 
  | w1/=w2    = (l1,w1):amalgamate ((l2,w2):rest)
  | otherwise = amalgamate ((l1++l2,w1):rest)
  
shorten :: [(Int,Word)] -> [(Int,Word)]  
shorten = filter (\(l,w) -> length w>3) 
-- sizer = (>3).length.snd    
              
linesIndex :: Doc -> [String]
linesIndex 
  = lines >.> numLines >.> allListWords >.> 
    shorten >.> sortL >.> makeList >.> amalgamate  >.> rangeWords >.>
	map (\(nums,word) -> uncapHead word++" \t"++ makeRange nums)

string = "cathedral doggerel cathedral\nbattery doggerel cathedral\ncathedral"

printIndex :: (Doc -> [String]) -> String -> IO()
printIndex i = printPicture.i

intPairs :: [Int] -> [(Int,Int)]  
intPairs [] = []
intPairs [x] = [(x,0)]
intPairs [x,y] | y==x+1 = [(x,y)] | otherwise = [(x,0)]
intPairs (x:y:z:zs)
  | z==x+2 = (x,z):intPairs zs
  | y==x+1 = (x,y): intPairs (z:zs)
  | otherwise = (x,0): intPairs (y:z:zs) 

nRange :: [(Int,Int)] -> [(Int,Int)] 
nRange [] = [] 
nRange [(x,y)] = [(x,y)]
nRange ((a,b):(c,d):ys) 
  | b+2/=d = (a,b):nRange ((c,d):ys)
  | (d==0) && (b+1==c) = (a,c):nRange ys
  | otherwise      = nRange ((a,d):ys)

rangeWords = map (\(x,y) -> ((nRange . intPairs) x,y)) 

makeRange :: [(Int,Int)] -> String
makeRange [] = []
makeRange [(x,0)] = show x
makeRange [(x,y)] = show x++"-"++show y
makeRange ((x,y):xs) 
  | y==0 = show x++", "++makeRange xs
  | otherwise = show x++"-"++show y++", "++makeRange xs

lengthEqual x = length .filter (==x).splitWords

lengthWord st =  [(lengthEqual xs st,xs) | xs<-splitWords st] 

sortWords :: [(Int,Word)] -> [(Int,Word)]
sortWords [] = []
sortWords ((n1,w1):xs) = sortWords smaller ++ [(n1,w1)] ++ sortWords larger
  where
  smaller = [(n,w) | (n,w)<-xs, w<w1]
  larger  = [(n,w)| (n,w)<-xs, w1<w]

uncapHead :: Word -> Word
uncapHead (x:xs)= if isLower x then x:xs else (toEnum (fromEnum x+32) :: Char):xs

occurenceIndex :: Doc -> [String] 
occurenceIndex 
  = lengthWord >.> sortWords >.> shorten
    >.> map (\(num,word) ->  uncapHead word++" \t"++ show num)
{-
[m..n] 
  | m>n = []
  | otherwise = m:[m+1..n]  (..)
-}
 
isPalin [] = False
isPalin xs = upperCase == reverse upperCase
  where
  upperCase = [toUpper x | x <- xs, isAlphaNum x]

subseq :: String -> String -> Bool
subseq [] _ = True
subseq (_:_) [] = False 
subseq (x:xs) (y:ys) = subseq (x:xs) ys || frontseq (x:xs) (y:ys)


frontseq :: String -> String -> Bool
frontseq [] _ = True
frontseq (_:_) [] = False 
frontseq (x:xs) (y:ys) = (x==y) && frontseq xs ys  
 
subst xs [] zs = xs++zs
subst [] _ _  = [] 
subst (x:xs) (y:ys) (z:zs) 
  | frontseq (y:ys) (x:xs) = z:subst xs ys zs
  | otherwise              = x:subst xs (y:ys) (z:zs)  -- bad program
  
  
