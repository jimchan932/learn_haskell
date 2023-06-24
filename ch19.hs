module Ch19 where  
import Pictures (horse)
import Ch17 (Expr(..), Ops(..)) 
import Control.Monad (liftM, liftM2)
import Text.Parsec hiding (Empty)
import Text.Parsec.Language 
import Text.Parsec.String 
import Text.Parsec.Token 
import Text.Parsec.Expr 
import Test.QuickCheck
import Data.Ix 
type Picture = [[Char]]

substChar :: Char -> Char -> Picture -> Picture 
substChar x y pic = [[if char == x then y else char | char <- line ] 
                    | line <- pic]

data Pic = Horse 
         | Above Pic Pic
         | Beside Pic Pic  
	 | FlipH Pic 
	 | FlipV Pic 
	 | Invert Pic 
	 | SubstChar Char Char Pic 
		 deriving Show 
					
interpretPic :: Pic -> Picture 
interpretPic Horse = horse 
interpretPic (Above pic1 pic2) = (++) (interpretPic pic1) (interpretPic pic2)
interpretPic (Beside pic1 pic2) = zipWith (++) (interpretPic pic1) (interpretPic pic2)
interpretPic (FlipH pic) = reverse (interpretPic pic)
interpretPic (FlipV pic) = map reverse (interpretPic pic)
interpretPic (SubstChar x y pic) = substChar x y (interpretPic pic)

tidyPic :: Pic -> Pic 
tidyPic (FlipV (FlipV pic)) = tidyPic pic 
tidyPic (FlipV (FlipH pic)) = FlipH (tidyPic (FlipV pic))
tidyPic (FlipV (Above pic1 pic2)) 
  = Above (tidyPic (FlipV pic2)) (tidyPic (FlipV pic2))
tidyPic (FlipH (FlipH pic)) = tidyPic pic 
tidyPic (FlipH (FlipV pic)) = FlipV (tidyPic (FlipH pic)) 
tidyPic (FlipH (Above pic1 pic2))
  = Above (tidyPic (FlipH pic1)) (tidyPic (FlipH pic2))
tidyPic (FlipH (Beside pic1 pic2)) 
  = Beside (tidyPic (FlipH pic1)) (tidyPic (FlipH pic2))

type RegExp = String -> Bool 

infixr 5 :|:
infixr 7 :*: 

data RE = Eps
        | AllChars 
        | Ch Char 
        | RE :|: RE 
        | RE :*: RE
        | St RE
        | Plus RE
        | Q RE 
        | Times RE (Int,Int)
	deriving Eq
	
allChars = range('0','9') ++ range('A','Z') ++ range('a','z')

enumerate :: RE -> [String]
                                        
enumerate Eps = [""]

enumerate AllChars = map (\ch -> [ch]) allChars 

enumerate (Ch ch) = [[ch]]



enumerate (re1 :|: re2)  
  = enumerate re1 `interleave` enumerate re2

enumerate (re1 :*: re2)
  = enumerate re1 `cartesian` enumerate re2

enumerate (St re)
  = result 
  where 
   result =                                      
     [""] ++ (enumerate re `cartesian` result)

enumerate (Plus re) 
  = enumerate re `cartesian` enumerate (St re)
  
enumerate (Q re) 
  = [""] ++ enumerate re

enumerate (Times re (a,b)) =
  if a>b 
     then [concat (replicate a enums) | enums <- enumerate re]
     else  [concat (replicate range enums) | enums <- enumerate re, range <-range(a,b)]

interleave :: [a] -> [a] -> [a] 
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs
 
cartesian :: [[a]] -> [[a]] -> [[a]]
cartesian [] ys = []
cartesian (x:xs) ys 
  = [x++y  |  y <- ys] `interleave` cartesian xs ys 
 
plus :: RE -> RE 
plus re = (re :*: St re) 

a = Ch 'a' 
b = Ch 'b'

anbn = Eps :|: (a :*: anbn :*: b)

simplify :: RE -> RE
simplify (St (St re))        = simplify (St re)
simplify (St (Plus re))      = simplify (St re)
simplify (Plus (St re))      = simplify (St re) 
simplify (Plus (Plus re))    = simplify (Plus re)
simplify (St (Q re))         = simplify (St re) 
simplify (Q (St re))         = simplify (St re)
simplify (Q (Q re))          = simplify (Q re)
simplify (Ch _ :|: AllChars) = AllChars
simplify (AllChars :|: Ch _) = AllChars
simplify (re1 :|: re2) = 
  if sre1 == sre2  then sre1 else (sre1 :|: sre2) 
    where 
    sre1 = simplify re1; sre2 = simplify re2;
simplify re = re 

starC :: RE -> RE 
starC (St re) = re 
starC (Plus re) = re 
starC (Q re) = re 
starC re = St re

data Shape = Rectangle Float Float 
           | Triangle Float Float Float 
	   deriving Show  

triangleC :: Float -> Float -> Float -> Shape
triangleC a b c 
  | allPositive && triEq = Triangle a b c 
  | otherwise = error $ "Illegal triangle: " ++ show a ++ ", " ++ show b ++ ", " ++ show c 
    where triEq = a + b > c && a + c > b && b + c > a  
          allPositive = a > 0 && b > 0 && c > 0 

interp :: RE -> RegExp 

interp Eps = (=="") 

interp AllChars = (`elem` (enumerate AllChars))

interp (Ch ch) = (==[ch])	 

interp (re1 :|: re2) = 
  \x -> interp re1 x || interp re2 x 

interp (re1 :*: re2) = 
  \x -> or [interp re1 y && interp re2 z | (y,z) <-splits x]
  where splits x = [splitAt n x | n <- [1..length x]]
                                                                    
interp (St re) = interp (Eps :|: (re :*: St re))     

interp (Plus re) = interp (re :*: St re)

interp (Q re) = \x -> x==[] || interp re x

interp (Times re (a,b)) = (`elem` enumerate (Times re (a,b)))

parseRE :: String -> RE 
parseRE st = 
  case result of 
    (Left e) -> error $ show e 
    (Right re) -> simplify re 
  where result = parse parseRegexp "" st

lexer :: TokenParser ()                
lexer = makeTokenParser $ javaStyle { opStart = oneOf "|*+?-{}"
                                    , opLetter = oneOf "|*+?-{}"
                                    , reservedNames = ["#e","."]}
                                         
parseRegexp, parseTerm, 
  parseEpsilon, parseAllChars,
  parseLiteral :: Parser RE
 
parseRegexp = (flip buildExpressionParser) ( try parseTimesRE <|> parseTerm) $ [ 
            [ Postfix (reservedOp lexer "*" >> return St)
            , Postfix (reservedOp lexer "+" >> return Plus)
            , Postfix (reservedOp lexer "?" >> return Q ) ]
          , [Infix (reservedOp lexer "" >> return (:*:)) AssocRight ]
          , [Infix (reservedOp lexer "|" >> return (:|:)) AssocRight ] 
          ]                      

parseTerm = do 
  whiteSpace lexer 
  term <- parens lexer parseRegexp 
          <|> parseLiteral
          <|> parseEpsilon 
          <|> parseAllChars
  whiteSpace lexer 
  return term

parseLiteral = oneOf allChars 
               >>= return . Ch

parseEpsilon = reserved lexer "#e"
               >> return Eps

parseAllChars = reserved lexer "."
                >> return AllChars

parseTimesRE = do 
  re <- parens lexer parseRegexp <|> parseTerm
  r <- braces lexer parseRange
  return (Times re r)
  where 
  parseRange = do 
    a <- natural lexer
    comma lexer 
    b <- natural lexer 
    return (fromInteger a, fromInteger b)

instance Show RE where
  show Eps           = "#e" 
  show AllChars      = "."
  show (Ch ch)       = [ch]
  show (re1 :|: re2) = "(" ++ show re1 ++ "|" ++ show re2 ++ ")"
  show (re1 :*: re2) = show re1 ++ show re2 
  show (St re)       = "(" ++ show re ++ ")*"
  show (Plus re)     = "(" ++ show re ++ ")+" 
  show (Q re)        = "(" ++ show re ++ ")?"
  show (Times re (a,b)) = show re ++ "{" ++ show a ++ "," ++ show b  ++ "}"  

prop_inverse st = st == show (parseRE st) 

data Card = Card Int String 
          deriving (Eq,Show)

instance Arbitrary Card where 
  arbitrary = 
      do int <- arbitrary 
         string <- arbitrary 
	 return (Card int string)

data Info = Number Int | Email String 
          deriving (Eq, Show)

instance Arbitrary Info where 
  arbitrary = 
      do bool <- arbitrary 
         if bool 
	    then do 
	      int <- arbitrary 
	      return (Number int) 
	    else do 
	      string <- arbitrary 
	      return (Email string)

data List a = Empty | Cons a (List a)
            deriving (Eq,Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do 
      list <- arbitrary 
      return $ foldr Cons Empty list
	     
instance Arbitrary Expr where 
  arbitrary = sized arbExpr

{-
arbExpr :: Int -> Gen Expr
arbExpr 0 = 
    do int <- arbitrary 
       return (Lit int) 
arbExpr n 
  | n>0 = 
      do pick <- choose (0,2 :: Int)
         case pick of 
	   0 -> do int <- arbitrary 
	           return (Lit int) 
           1 -> do left <- subExpr 
	           right <- subExpr 
		   return (Op Add left right) 
           2 -> do left <- subExpr 
	           right <- subExpr 
		   return (Op Sub left right) 
           where 
	     subExpr = arbExpr (div n 2) -} 

arbExpr 0 = liftM Lit arbitrary 
arbExpr n = frequency 
    [(1, liftM Lit arbitrary),
     (2, liftM2 (Op Add) subExpr subExpr),
     (2, liftM2 (Op Sub) subExpr subExpr)]
       where
         subExpr = arbExpr (div n 2)

