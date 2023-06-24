{-# OPTIONS_GHC -fno-warn-amp #-}
module Ch18 where
import Prelude hiding (repeat,iterate,map)
import Data.Time
import System.Locale
import System.IO.Unsafe
import System.IO hiding (putStrLn)
import Ch12 (isPalin)
import Ch17 
import Store
import Data.Maybe
import Ch14_2 (Tree(..),mapTree,List(..))
import OrdSets 


readWrite = do getLine
               putStrLn "one line read"

readEcho = getLine >>= \line ->
           putStrLn ("line read: "++line)
	   
getInt :: IO Integer
getInt = getLine >>= \n ->
         return (read n :: Integer)

sumAcc s [] = s
sumAcc s (n:ns)
  = if n==0
       then s
       else sumAcc (s+n) ns

sumInteract
  = do putStrLn "Enter integer one per line"
       n <-sumInts 0
       putStrLn ("The sum is "++show n)

fmap'' :: (a -> b) -> IO a -> IO b
fmap'' f a = a >>= \x ->
            return (f x)  

repeat :: IO Bool -> IO () -> IO ()
repeat test oper
  = do x<-test
       if x
       then return () 
       else do oper 
               repeat test oper
	       
while test action a 
  = do res <- test a
       if res
          then do action a
                  while test action a
	  else return ()

accumulate :: [IO a] -> IO [a]
accumulate ls
  = case ls of
      [] -> return []
      (oper:xs) -> do x<-oper
                      rest<-accumulate xs
                      return(x:rest)

sequence' [] = return () 
sequence' (oper:xs)
  = do oper
       sequence' xs

copyInteract :: IO ()
copyInteract =
    do
       hSetBuffering stdin LineBuffering
       copyEOF
       hSetBuffering stdin NoBuffering

copyEOF =
    do eof <-isEOF
       if eof
       then return()
       else do line <- getLine
               putStrLn line
               copyEOF
	       
readTest = readFile "TestFile.txt"

listIOprog :: String -> String
listIOprog = unlines.map reverse.lines

interact' :: (String -> String) -> IO ()
interact' f = do s<-getContents
                 putStr (f s)

randomInt :: Int -> IO Int
randomInt n =
    do
       time<-getCurrentTime
       return((`rem` n) $ read $ take 6 $
                          formatTime defaultTimeLocale "%q" time)

randInt :: Int -> Int 
randInt = unsafePerformIO. randomInt

data Move = Rock | Paper | Scissors deriving (Show,Enum) 

type Strategy = [Move] -> Move 
sRandom :: Strategy
sRandom _ =  toEnum (randInt 3)

type StrategyM = [Move] -> IO Move

rStratM _ = do n<-randomInt 3
               return(toEnum n :: Move)

fileSum name = do nums<-readFile name
                  return(sum $ map read $ words nums)

listSumInts = show . foldr (+) 0 . map read . lines . takeWhile (/='0')

listIOparse = unlines . map (show . topLevel commandParse Null) . lines

eval :: Expr -> Store -> Integer
eval (Lit n) _ = n
eval (Var v) st = value st v
eval (Op op e1 e2) st
  = opValue op v1 v2
    where
    v1 = eval e1 st
    v2 = eval e2 st 

opValue :: Ops -> Integer -> Integer -> Integer
opValue op = fromJust (lookup op opList)
  where
  opList = [(Mod,mod),(Div,div),(Mul,(*)),(Add,(+)),(Sub,(-))]

command :: Command -> Store -> (Integer, Store)
command Null st = (0,st) 
command (Eval e) st = (eval e st,st) 
command (Assign v e) st
  = (val,newSt)
    where
    val   = eval e st
    newSt = update st v val

calcStep :: Store -> IO Store
calcStep st
  = do line<-getLine
       let comm = commLine line
       let (val,newSt) = command comm st
       print val
       return newSt

calcSteps :: Store -> IO ()
calcSteps st =
  do eof<-isEOF
     if eof
        then return()
	else do newSt<-calcStep st
	        calcSteps newSt

mainCalc :: IO ()
mainCalc =
    do hSetBuffering stdin LineBuffering
       calcSteps initial
       hSetBuffering stdin NoBuffering
{-           
infixr 5 >>=

(>>=) :: m a -> (a -> m b) -> m b
m >>= f = do res <- m
             f res-}

sumInts :: Integer -> IO Integer
sumInts s = getInt >>= \n ->
            if n==0 
               then return s
               else sumInts (s+n)


addOneInt :: IO ()

addOneInt = getLine >>= \line ->
            print (1 + read line :: Int)
                          
interact f = getContents >>= \line ->
             putStr (f line)  

(>@>) :: Monad m => (a -> m b) -> (b  -> m c) -> (a -> m c)
f >@> g = \x -> f x >>= g 
  
newtype MP a b = MP {mp :: Parse a b}

instance Monad (MP a) where
  return x = MP (succeed x)
  fail s   = MP none
  (MP pr) >>= f  
    = MP (\st -> concat [mp (f x) rest | (x,rest) <- pr st])
	
opExpParseM :: MP Char Expr 
opExpParseM = do tokenM '(';
   	             e1 <- parseExprM;
			     bop <- spotM isOp; 
				 e2 <- parseExprM;
				 tokenM ')'; 
				 return (Op (charToOp bop) e1 e2)

spotM :: (a -> Bool) -> MP a a 
spotM = \p -> MP (spot p)	 

parseExprM :: MP Char Expr
parseExprM = MP parser
	
tokenM :: Eq a => a -> MP a a  
tokenM x = spotM (==x) >>= return

mapL f NilL = NilL
mapL f (x ::: xs) = f x ::: mapL f xs

(+++) :: List a -> List a -> List a 
NilL +++ xs = xs
(x ::: xs) +++ ys = x ::: xs +++ ys 

concatL NilL = NilL 
concatL (x ::: xs) = x +++ concatL xs 

instance Monad List where
  return x      = x ::: NilL 
  fail _        = NilL
  NilL >>= _    = NilL
  xs >>= f      = concatL (mapL f xs)
  
newtype Identity a = Identity {identity :: a}  

instance Monad Identity where
  return x           = Identity x 
  fail st            = Identity (error st)
  (Identity x) >>= f = f x 
  
infixr 1 >=>
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c 
(a >=> b) x = a x >>= b     

instance Show a => Show (Identity a) where
  show = show . identity
  
fmap' :: Monad m => (a -> b) -> m a -> m b 
fmap' f m 
  = do x <- m 
       return (f x)
        
join' :: Monad m => m (m a) -> m a 
join' m = do x <- m
             x 

data Error a = OK a | Error String deriving Show

instance Monad Error where
  return x          = OK x   
  fail              = Error
  (OK x) >>= f      = f x
  (Error st) >>= _  =  Error st  

instance Monad Tree where
  return x             = Node x Nil Nil 
  fail _               = Nil
  (Node x t1 t2) >>= f = f x
  
map f xs = [f x | x <- xs]  
  
join :: [[a]] -> [a]  
join xss = [x | xs <- xss, x <- xs]
  
instance Functor Tree where
  fmap = mapTree
  
sumTree :: Tree Integer -> Identity Integer    
sumTree Nil = return 0
sumTree (Node n t1 t2) 
  = do num <- return n
       s1 <- sumTree t1
       s2 <- sumTree t2 
       return (num + s1 + s2)	   
	   
sTree :: Tree Integer -> Integer    
sTree = identity . sumTree	   

numberTree :: Eq a => Tree a -> State a (Tree Integer)
numberTree Nil = return Nil 
numberTree (Node x t1 t2)
  = do num <- numberNode x 
       nt1 <- numberTree t1 
       nt2 <- numberTree t2
       return (Node num nt1 nt2)

type Table a = [a]	   
	   
data State a b = State (Table a -> (Table a,b))

instance Monad (State a) where
  return x         = State (\tab -> (tab,x))
  (State st) >>= f 
    = State (\tab -> let 
	                 (newTab,y)    = st tab;
					 (State trans) = f y;
					 in 
             		 trans newTab)
  
numberNode :: Eq a => a -> State a Integer 
numberNode x = State (nNode x)

lookup' :: Eq a => a -> Table a -> Integer
lookup' x table = count x (0,table)
  where
  count x (n,(y:ys)) 
    | x==y  = n 
    | x/=y  = count x (n+1,ys)
  
nNode :: Eq a => a -> (Table a -> (Table a,Integer))
nNode x table  
  | elem x table = (table , lookup' x table)
  | otherwise    = (table ++ [x], integerLength table)
    where 
	integerLength = toInteger . length 
	
runST :: State a b -> b
runST (State st) = snd (st [])

numTree, numTree' :: Eq a => Tree a -> Tree Integer 
numTree = runST . numberTree

numTree' = convertTree []  

convertTree :: Eq a => [a] -> Tree a -> Tree Integer  
convertTree _ Nil = Nil 
convertTree xs (Node x t1 t2) = Node n nt1 nt2 
  where
  (table,n) = nNode x xs
  nt1 = convertTree table t1 
  nt2 = convertTree table t2   

numberSteps :: Expr -> Identity Integer
numberSteps (Var _) = return 0
numberSteps (Lit _) = return 0 
numberSteps (Op op e1 e2) 
  = do n1 <- numberSteps e1 
       n2 <- numberSteps e2 
       return (1 + n1 + n2)	 

nSteps = identity . numberSteps	   


		   
