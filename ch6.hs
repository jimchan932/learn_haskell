module Ch6 where
import Prelude hiding (lookup)
import Test.QuickCheck hiding (scale)
import Data.List hiding (lookup)
import Data.Time

type Picture = [[Char]]

flipH :: Picture -> Picture
flipH = reverse

flipV :: Picture -> Picture
flipV = map reverse 

above :: Picture -> Picture -> Picture
above = (++)

printPicture :: Picture -> IO ()
printPicture = putStrLn.unlines

p = printPicture

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

beside :: Picture -> Picture -> Picture
beside a b = [x++y |(x,y) <- zip a b]
  
invertLine :: String -> String
invertLine line = [if ch == '.' then '#' else '.' | ch<-line]
testInvertLine = invertLine "......######..##...#######....##.#.######.......####.#.#"

invertColour :: Picture -> Picture
invertColour pic = [invertLine x | x<-pic]

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV a b = flipV (a `above` b) == above (flipV a) (flipV b)

prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH a b = flipH (a `above` b) == above (flipH a) (flipH b)


compareFlipH = p (flipH (horse `above` horse) `beside` above (flipH horse) (flipH horse)) 
-- what a difference

stickMan :: Picture
stickMan = ["----------------------------------",
            "----------------------------------",
            "-------------EEEEEE---------------",
            "-----------EEEE---EEE-------------",
            "----------EE--E---E-EEE-----------",
            "----------EE---------EE-----------",
			"-----------EE--EEEE--EE-----------",
			"------------EEEE---EEE------------",
			"---------EE----EEEEE-----EE-------",
			"---------EEEEEEEEEEEEEEEEEE-------",
			"----------------EEE---------------",
			"-----------------E----------------",
			"-----------------EE---------------",
			"-----------------EE---------------",
			"-----------------EE---------------",
			"----------------EEEE--------------",
			"---------------EE--EEE------------",
			"--------------EE-----EEE----------",
			"------------EEE-------EEE---------",
			"------------EE----------EE--------",
			"----------------------------------"]
  
square :: Int -> [Char] -> Picture
square n s = replicate n (duplicate n s) 

isSquare :: Picture -> Bool
isSquare p = eachLength p == length p
star = "*"
  
superimposeLine :: String -> String -> String
superimposeLine a b = [if (y==x)&&(y=='.') then '.' else '#' | (x,y)<-zip a b]
 -- zips the ith element of the 2 pictures and changes its char  
  
superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 = [superimposeLine x y | (x,y)<-zip p1 p2]

scaleLine :: Int -> String -> String
scaleLine n s = concat [replicate n x | x<-s] -- replicate turns a char into string,
                                              -- and turns a string into a list,
testSLine = scaleLine 3 "Hello World"         -- thus, *** we need to concat it

scale ::  Int -> Picture -> Picture
scale n p = [scaleLine n y | y<-p]  
   -- think!
   
testScale = p (scale 2 horse)

rag :: Picture -> Picture
rag p = above (p `beside` p ) p -- Bad example

  
eachLength = length.head

fourPics n = left `beside` right
  where
  stack a = above a (invertColour (flipV a)) 
  left = stack (invertColour n)
  right = stack n  


fourPics2 n = above up down
  where
  stack a = a `beside` (invertColour (flipV a))
  up = stack n
  down = stack (invertColour n)

combineHorses i 
  | i==1 = p (beside (fourPics horse) (fourPics2 horse)) 
  | i==2 = p (beside (fourPics2 horse) (fourPics horse)) 
  | otherwise = error "this version is not defined"

type BoolPic = [[Bool]]

bList :: BoolPic
bList = [[True,False,True,False],[True,True,True,True],[True,False,False,False],[False,True,True,False]]
  -- disadvantage: Indirect, advantage: only evaluates '#' or '.'
  
bString b = [if x==True then '#' else '.' | x<-b]  
showB b = p [bString x | x<-b]

bPic = showB bList
getColumn :: Int -> Picture -> String
getColumn n p = [x!!n | x<-p]  -- get the nth element of each item in the list,
                               -- 0 to (n-1)
tPic = [".##.",
        ".#.#",
		".###",
        "####"]	

rotate90 p = flipV [getColumn n p | n<-[0..(length p-1)]]  -- it works baby!  

rotate180 = rotate90.rotate90

invert :: Picture -> Picture
invert = flipV.flipH 

prop_180 p = invert p == rotate180 p

rotate270 = rotate180.rotate90 
	
ragHorse = (horse `beside` horse) `above` horse
newLines :: Picture -> Picture
newLines p = [x++"\n" | x<-p]

printColumn = printPicture.rotate90

type RunStr = [(Int,Char)]
type RunLength = [RunStr]

picCode :: RunLength
picCode = [[(4,'.'),(3,'1')],[(2,'#'),(5,'2')],[(1,'$'),(6,'3')],[(5,'@'),(2,'4')]]

tStr :: Int -> RunStr
tStr n = picCode!!n

runStr :: RunStr -> String
runStr s = concat [replicate a b | (a,b)<-s]  
           {- concats every line -} 
runLength :: RunLength -> Picture
runLength l = [runStr x | x<-l, inequal (test x)]
  where
  inequal b = not (and [test2 c b | c<-b]) -- test all elements whether they are the
  test2 c s = elem c (delete c s)    -- SAME elements with the others in the list 
  test x = [snd y | y<-x]  -- *****
  -- finally!
  
type RunLength2 = (Int,RunStr)

picCode2 :: [RunLength2]
picCode2 = zip [2,5,3,4] picCode

runPic :: [RunLength2] -> Picture
runPic p = concat [replicate x y |(x,y)<-rP p] 
  where      -- never fear programming 
  rP p = [(x,runStr y) | (x,y)<-p] -- apply the fun runLength, runStr to this

type Position = (Int,Int) -- (x,y)
type Image = (Picture,Position)
	
duplicate :: Int -> String -> String
duplicate n s
  | n==1  = s
  | n>1   = s++duplicate (n-1) s 

plusSpace :: Position -> Picture  
plusSpace (x,y) = concat [replicate y (duplicate x " ")]

makeImage :: Picture -> Position -> Image
makeImage p (x,y) =  (above (beside upperLeft p) (beside (plusSpace (x,y)) lowerRight),(x,y)) 
  where
  upperLeft  = plusSpace (x,length p)
  lowerRight = plusSpace (eachLength p,y) 
  
changePosition :: Image -> Position -> Image
changePosition i nP = makeImage (fst i) nP

moveImage :: Image -> Int -> Int -> Image
moveImage (i,(a,b)) x y = makeImage i (a+x,y+b)

coordinates :: Image -> Position
coordinates img = snd img

printImage :: Image -> IO()
printImage = putStrLn.unlines.fst 

showImg (p,(x,y)) = fst (makeImage p (x,y)) 

rotation :: Transform -> Picture
rotation (Clockwise (i,(x,y))) =  above (beside  upperLeft i) (beside newPosition lowerRight)   
  where
  newPosition = showImg (rotate180 i,(div x 2,div y 2))
  upperLeft  = plusSpace (div x 2,length i)
  lowerRight =  plusSpace (eachLength i,(div y 2))

data Transform = Clockwise Image | Clockverse Image | Right Image | Left Image deriving (Show,Eq)



type Name = String
type Price = Float
type BarCode = Int 
-- Keep it up!
type Database = [(BarCode,Name,Price)]
type TillType = [BarCode]
type BillType = [(Name,Price)]

codeIndex :: Database
codeIndex = [(4719, "Fish Fingers", 121),
             (5643, "Nappies", 1010),
			 (3814, "Orange Jelly", 56),
			 (1111, "Hula Hoops", 21),
			 (1112, "Hula Hoops (Giant)", 133),
			 (1234, "Dry Sherry, 1lt", 540)]

formatPence :: Price -> Float
formatPence p = p/100  
  

formatLine :: (Name,Price) -> String
formatLine (n,p) = addDots (n,show (formatPence p))++"\n"
  where
  addDots (a,b) = a++replicate (toAdd (a,b)) '.'++b
  toAdd (a,b) = 29-length a-length b    

makeTotal :: BillType -> Float
makeTotal bill = sum [y | (x,y)<-bill]

prop_Total l = makeTotal l == sum [snd x | x<-l]

formatInfo :: (Price,Price) -> String
formatInfo (t,p) 
  | p>t       = "\n"++formatLine ("Total",t)++pay'NChange p t
  | p==t      = formatLine ("Cash",p)    
    where
	pay'NChange p t = formatLine ("Cash",p)++formatLine("Change",p-t)


spaces x = replicate (div (30-length x) 2) ' '

bill = [(y,z) |(x,y,z)<-codeIndex]

centeredTitle = "\t"++title++"\t\n\n"
  where
  title = "Haskell stores"
  
formatBill :: Float -> BillType -> IO ()
formatBill payment b = if payment<makeTotal b then error "Not enough to pay" 
                       else do putStrLn (centeredTitle++showBill b++formatDiscount (makeDiscount b)++formatInfo(makeTotal b, payment))  
  where
  showBill b = concat [formatLine x | x<-b, x/= ("Unknown Item",0)]
  -- the function does not format for  ("Unknown Item",0) 
  

look :: Database -> BarCode -> (Name,Price)
look dBase code = if elem code barCodes then head [(y,z) | (x,y,z)<-dBase, code==x] 
else ("Unknown Item",0)
  where
  barCodes = [x | (x,y,z)<-dBase]

lookup :: BarCode -> (Name,Price)
lookup code = look codeIndex code 

makeBill :: TillType -> BillType
makeBill b = [lookup x | x<-b]

makeDiscount :: BillType -> Float
makeDiscount b = sum [ if x=="Dry Sherry, 1lt" then 1 else 0 | (x,y)<-b]
  
formatDiscount :: Price -> String
formatDiscount p = "\nDiscount"++addDots ("\nDiscount",discount)++discount++"\n"
  where
  discount = show p ++ ".00"   
  addDots (a,b) = replicate (30-length a-length b) '.'

addProduct base a (b,c)  = [(a,b,c)] ++ [(x,y,z)|(x,y,z)<-base,x/=a]  
  
fibP :: Integer -> (Integer,Integer)
fibP 0 = (0,1)
fibP n = (y,x+y)
       where 
       (x,y) = fibP (n-1)
	   
rightTriangle :: Int -> Picture
rightTriangle n = [if n<=40 then replicate x star else error "Number too large"| x<-take n [1,3..79]]
  where
  star = '*'
  
isosTriangle :: Int -> Picture
isosTriangle n = [space x ++x++space x| x<-rightTriangle n]
  where
  lastL = length (last (rightTriangle n))
  space i = replicate (div (lastL-length i) 2) ' ' 

diamond :: Int -> Picture
diamond n = makeDiamond (isosTriangle n)
  where
  makeDiamond n = above n (flipH (init n))   

printDiamond :: Int -> IO()
printDiamond i = p (diamond i) 

lineLength = 80

data Suit = Diamonds | Clubs | Hearts | Spades
 deriving (Show,Eq,Ord)

data Value  = Int | Jack | Queen | King | Ace deriving (Show,Eq,Ord) 
data Deck = Suit Value deriving (Show,Eq,Ord) 
data Direction = North | East | South | West deriving (Show,Eq,Ord)

iSort2 [] = []
iSort2 (y:ys) = ins y (iSort2 ys)

ins x [] = [x]
ins x (y:ys) 
  | x>=y = x:(y:ys)
  | otherwise = y: ins x ys 
  
