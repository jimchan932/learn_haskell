module Ch8 where
import Prelude hiding (putStrLn, cycle, print)
import qualified Prelude
import Test.QuickCheck
import Ch4 hiding (result)
import Data.Time
import System.Locale
import System.IO.Unsafe
import System.IO hiding (putStrLn, print)
import Data.Char
type Tournament = ([Move],[Move])

outcome :: Move -> Move -> Integer 
outcome a b 
  | b==beat a  = -1
  | b==lose a  =  1
  | b==a       =  0
  
tournamentOutcome :: Tournament -> Integer
tournamentOutcome (_,[])  = 0
tournamentOutcome ([],_)  = 0
tournamentOutcome ((x:xs),(y:ys)) = outcome x y+ tournamentOutcome (xs,ys)

type Strategy = [Move] -> Move -- the type of functions

rock, paper, scissors :: Strategy 
rock _     = Rock
paper _    = Paper
scissors _ = Scissors

cycle :: Strategy
cycle moves 
  = case (length moves) `rem` 3 of 
    0 -> Rock
    1 -> Paper 
    2 -> Scissors

convertToMove :: Integer -> Move
convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors

-- Random choice of Move
randomStrategy :: Strategy
randomStrategy _ = convertToMove $ randInt 3

randomInt :: Integer -> IO Integer
randomInt n = 
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)
-- Extract the random number from the IO monad, unsafely!
randInt :: Integer -> Integer

randInt = unsafePerformIO . randomInt 
	
echo :: Strategy 
echo (latest:rest) = latest
echo []            = Rock 

winStrategy :: Strategy
winStrategy l = beat (echo l)

loseStrategy :: Strategy        -- lose is better since the name of the game is Rock Paper Scissors
loseStrategy l = lose (echo l)  -- the player may easily understand that winStrategy returns the move 
                                -- that beats the head of his [Move]
newStrategy :: Strategy 
newStrategy (x:xs)
  | x==head xs = lose x  -- we assume that the player will not repeat x
  | otherwise  = randomStrategy (x:xs) 

print :: Show a => a -> IO()
print = putStrLn . show

putStrLn :: String -> IO()
putStrLn str = do putStr str
                  putStr "\n"
	-- putStrLn = putStr.(++"\n")	
				  
reverse2lines = do lineA <- getLine
                   lineB <- getLine
                   let revA = reverse lineA
                   let revB = reverse lineB
                   putStrLn revB
                   putStrLn revA	
				   
put4times :: String -> IO()
put4times str = do putStrLn str
                   putStrLn str
                   putStrLn str
                   putStrLn str

putNtimes 1 str = do putStrLn str
putNtimes n str = do putStrLn str
                     putNtimes (n-1) str 

read2lines :: IO ()
read2lines  = do getLine 
                 getLine  
                 putStrLn "Two lines read."
getNput :: IO()
getNput = do line <- getLine
             putStrLn line

getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)    
			
add2 :: IO Integer
add2 = do putStrLn "Input 2 numbers to return their sum"
          n1 <- readLn
          n2 <- readLn		  
          return (n1+n2)
		  
copy :: IO()
copy = 
    do line <- getLine
       putStrLn line
       copy		  
		  
copyN :: Integer -> IO ()
copyN n = if n <= 0
          then return () 
          else do line <- getLine
                  putStrLn line
                  copyN (n-1)			  
				  
copyEmpty :: IO()
copyEmpty =
    do line <- getLine
       if line == ""
          then return ()
          else do putStrLn line
                  copyEmpty				   
				   
copyCount :: Integer -> IO()
copyCount n = 
    do line <- getLine
       if line == ""
          then putStrLn (show n ++ " lines copied.")
          else do putStrLn line
                  copyCount (n+1)

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

showWc (a,b,c) = 
    do line <- getLine 
       if line==""
          then putStrLn ("  "++show a++"  "++show b++"  "++show c)
          else do let wordL = splitWords line
                  let charL = concat $ wordL
                  showWc (a+1,b + length wordL,c + length charL)			
wc = showWc (0,0,0)
				  
play :: Strategy -> IO()
play strategy = playInteractive strategy ([],[])					 
					 
playInteractive :: Strategy -> Tournament -> IO()
playInteractive s t@(mine,yours) =
    do 
      ch <- getChar
      if not (ch `elem` "rpsRPS") 
        then showResults t 
        else do let next = s yours 
                putStrLn ("\nI play: " ++ show next ++ " you play: " ++ [ch])
                let yourMove = convertMove ch
                playInteractive s (next:mine, yourMove:yours)
				 
showResults :: Tournament -> IO ()
showResults t = 
    do
      let res = result t
      putStrLn (case compare res 0 of
                  GT ->  "I won!"
                  EQ -> "Draw!"
                  LT -> "You won: well done!")
				 
result :: Tournament -> Integer
result = sum . map (uncurry outcome) . uncurry zip				 
				 
convertMove :: Char -> Move
convertMove 'r' = Rock
convertMove 'R' = Rock
convertMove 'p' = Paper
convertMove 'P' = Paper
convertMove 's' = Scissors
convertMove 'S' = Scissors

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x:xs) = qsort [l | l<-xs, l<=x]++[x]++qsort [g | g<-xs, g>x]
				
sort l = do x <- getInt
            if x==0
			then print (qsort l)
			else sort (x:l)


sortInts = sort []    

sumInteract n = do x <- getInt 
                   if x==0
				   then putStrLn ("sum = "++show n)
				   else sumInteract (n+x) 
				   
sumInts = sumInteract 0   

toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs
toData [] = []
toData (x:xs)
  | isAlphaNum x = x:toData xs
  | otherwise = toData xs  
isPalin :: String -> Bool
isPalin x = testString == reverse testString
  where
  testString = toUpperString $ toData x

checkPalines = do x <- getLine
                  let check = isPalin x
                  if x==""
                  then return ()
                  else do print check
                          checkPalines							
				  