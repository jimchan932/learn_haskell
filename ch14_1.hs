module Ch14_1 where
import Data.List
data Seasons = Spring | Summer | Autumn | Winter 
                                    deriving (Show,Eq,Ord,Enum,Read,Bounded)
type Borrower = String
type Book = String 
data Loan = Loan Borrower Book deriving (Show,Eq)   

newBase :: [Loan] 
newBase = [Loan "Jim" "Haskell: the craft of functional programming",
           Loan "Jim" "Steve Jobs by Walter Isaacson", 
    	   Loan "Ethan" "Programming in Objective-C (5th Edition)",
		   Loan "Vincent" "BASIC OF MATLAB and Beyond",Loan "Tom" "Haskell: the craft of functional programming",			   Loan "Tom" "Diary of a wimpy kid",Loan "Elsa" "DORK diaries",Loan "Elsa" "Diary of a wimpy kid",
	       Loan "Vincent" "Harry Potter and the DEATHLY HALLOWS"]



data Item = Book String String  | CD String String | Video String deriving (Show,Eq,Ord)
type Collection = (Borrower,Item,Integer) 										 

exampleBase :: [Collection]
exampleBase  = [("Jim",Book "Haskell: the craft of functional programming" "Simon Thompson",14), 
                ("Jim",Book "Euclid's Elements" "Euclid",20),
                ("Jim",Book "Steve Jobs by Walter Isaacson" "Walter Isaacson",13),
			    ("Jack",Book "The Hobbit" "J. R. R. Tolkien",19),
				("Ethan",Book "Programming in Objective-C (6th Edition)" "Stephen G. Kochan",6),
			    ("Vincent",Book "BASIC OF MATLAB and Beyond" "Andrew Knight",23),
				("Tom",Book "Haskell: the craft of functional programming" "Simon Thompson",29), 
			    ("Tom",Book "Diary of a wimpy kid" "Jeff Kinney",30),
				("Elsa",Book "DORK diaries" "Rachel Renee Russell",153),
				("Elsa",Book "Diary of a wimpy kid" "Jeff Kinney",23),
			    ("Vincent",Book "Harry Potter and the DEATHLY HALLOWS" "J. K. Rowling",59),
			    ("Jack",CD "Kick-ass" "Brad Pitt",5),
				("Tom",CD "Kick-ass" "Brad Pitt",5),
				("Elsa",CD "Let it go" "Peter Del Vecho",2),
                ("Jim",Video "Epic Tyrion speech during trial",23)]

allItems :: [Collection] -> Borrower -> [Item]
allItems base pers = sort [x |(p,x,y) <- base, pers==p]

numBorrowed :: [Collection] -> Borrower -> Int
numBorrowed base = length.allItems base

items :: [Collection] -> String -> Borrower -> [Item]
items base "Book" pers = sort [Book name author | (p,Book name author,_) <- base, pers==p]
items base "CD" pers = sort [CD name author | (p,CD name author,_) <- base, pers==p]
items base "Video" pers = sort  [Video name | (p,Video name,_) <- base, pers==p]

allBeforeDue :: [Collection] -> [Item]
allBeforeDue base = [i | (p,i,n)<-base, isBefore (p,i,n)] 
  
isBefore (_,Book _ _,x) = 30>=x  
isBefore (_,CD _ _,x) = 7>=x  
isBefore (_,Video _,x) = 3>=x
  
beforeDue base pers = [i | (p,i,n)<-base, isBefore (p,i,n),pers==p]  

class Tell a where
  eval, size :: a -> Integer
  
infix 9 :*: 
infix 8 :/:
infix 7 :+:
infix 6 :-:
data InExpr = Num Integer | InExpr :+: InExpr | InExpr :-: InExpr | InExpr :*: InExpr |
              InExpr :/: InExpr 

instance Tell InExpr where
  eval (Num n) = n
  eval (e1 :*: e2) = eval e2*eval e1
  eval (_ :/: (Num 0)) = div0Ex 
  eval (e1 :/: e2) = div (eval e1) (eval e2) 
  eval (e1 :+: e2) = eval e2+eval e1
  eval (e1 :-: e2) = eval e1-eval e2

  size (Num n) = 0
  size (e1 :+: e2) = 1+size e1+size e2
  size (e1 :-: e2) = 1+size e1+size e2
  size (e1 :*: e2) = 1+size e1+size e2
  size (_ :/: (Num 0)) = div0Ex     
  size (e1 :/: e2) = 1+size e1+size e2
  
instance Show InExpr where
  show (Num n) = show n
  show (e1 :+: e2) = "("++show e1++"+"++show e2++")"
  show (e1 :-: e2) = "("++show e1++"-"++show e2++")"
  show (e1 :*: e2) = show e1++"*"++show e2
  show (_ :/: (Num 0)) = div0Ex
  show (e1 :/: e2)  = show e1++"/"++show e2

assoc :: InExpr -> InExpr
assoc ((e1 :+: e2) :+: e3) = assoc (e1 :+: (e2 :+: e3))
assoc (e1 :+: e2) = (assoc e1) :+: (assoc e2)
assoc (e1 :-: e2) = (assoc e1) :-: (assoc e2)
assoc (Num n) = Num n

showAssoc :: InExpr -> String
showAssoc = show.assoc 

data NTree = NilT | Node Integer NTree NTree deriving (Show)

depth, sumTree :: NTree -> Integer
sumTree NilT = 0
sumTree (Node n t1 t2) = n+sumTree t1+sumTree t2

depth NilT = 0
depth (Node n t1 t2) = 1+max (depth t1) (depth t2)

occurs :: NTree -> Integer -> Integer
occurs NilT _ = 0
occurs (Node n t1 t2) p
  | n==p = 1+occurs t1 p+occurs t2 p  
  | True = occurs t1 p+occurs t2 p
  
div0Ex = error "the expression has dividion by 0"   



data Person = Adult Name Address Bio | 
              Child Name 
data Bio = Parent String [Person] | 
           NonParent String 

data Name = Chan | Lee | Lau | Holmes | Lannister deriving (Show)
data Address = Uncle | Aunt | Grandpa | Grandma | Mom | Dad |
               Sister | Brother | Cousin | Wife | Husband | 
               Son | Daughter deriving (Show)  		   

instance Show Person where
  show (Adult nm ad bio) = show nm ++" "++ show ad ++ show bio
  show (Child nm) = show nm 
instance Show Bio where
  show (Parent st perList) = st ++ concatMap show perList
  show (NonParent st) = st 

leftTree, rightTree :: NTree -> NTree
leftTree NilT = NilT 
leftTree (Node n t1 t2) = t1

rightTree NilT = NilT
rightTree (Node n t1 t2) = t2 

elemTree :: Integer -> NTree -> Bool
elemTree _ NilT = False
elemTree n (Node x t1 t2) = x==n || elemTree n t1 || elemTree n t2

maxVal, minVal :: NTree -> Integer
maxVal (Node n NilT NilT) = n 
maxVal (Node n NilT t2) = max n (maxVal t2)
maxVal (Node n t1 NilT) = max n (maxVal t1)
maxVal (Node n t1 t2) = max n (max (maxVal t1) (maxVal t2))
maxVal NilT = 0
minVal (Node n NilT NilT) = n
minVal (Node n NilT t2) = min n (minVal t2)
minVal (Node n t1 NilT) = min n (minVal t1)
minVal (Node n t1 t2) = min n (min (minVal t1) (minVal t2))
minVal NilT = 0

reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node n t1 t2) = Node n (reflect t2) (reflect t1)

collapse, sortTrees :: NTree -> [Integer]

collapse NilT = []
collapse (Node n t1 t2) = collapse t1 ++ [n] ++ collapse t2
sortTrees = sort.collapse

data Expr = Lit Integer | Var Char | Op Ops Expr Expr 
            | If BExp Expr Expr 
	    
data Ops = Add | Sub | Mul | Div | Mod

data BExp = BoolLit Bool | And BExp BExp | 
             Not BExp | Equal BExp BExp |
			 Greater BExp BExp 
			 
instance Tell Expr where 
  size (Lit n) = 0
  size (Op Add e1 e2) = 1+size e1+size e2
  size (Op Sub e1 e2) = 1+size e1+size e2
  size (Op Mul e1 e2) = 1+size e1+size e2
  size (Op Div _ (Lit 0)) = div0Ex  
  size (Op Div e1 e2) = 1+size e1+size e2
  size (Op Mod _ (Lit 0)) = div0Ex     
  size (Op Mod e1 e2) = 1+size e1+size e2
  eval (Lit n) = n			   
  eval (Op Add e1 e2) = eval e1+eval e2
  eval (Op Sub e1 e2) = eval e1-eval e2
  eval (Op Mul e1 e2) = eval e1*eval e2
  eval (Op Div _ (Lit 0)) = div0Ex
  eval (Op Div e1 e2) = div (eval e1) (eval e2)
  eval (Op Mod _ (Lit 0)) = div0Ex 
  eval (Op Mod e1 e2) = mod (eval e1) (eval e2)
  eval (If b e1 e2) = if bEval b then eval e1 else eval e2
  
bEval :: BExp -> Bool  
bEval (BoolLit b) = b 
bEval (And b1 b2) = bEval b1&&bEval b2
bEval (Not b) = not (bEval b)
bEval (Equal e1 e2) = bEval e1==bEval e2
bEval (Greater e1 e2) = bEval e1>bEval e2


instance Show Expr where
  show (Lit n) = show n 
  show (Var c) = show c 
  show (Op Add e1 e2) = "("++show e1++"+"++show e2++")"
  show (Op Sub e1 e2) = "("++show e1++"-"++show e2++")"
  show (Op Mul e1 e2) = show e1++"*"++show e2
  show (Op Div _ (Lit 0)) = div0Ex  
  show (Op Div e1 e2) = show e1++"/"++show e2
  show (Op Mod _ (Lit 0)) = div0Ex
  show (Op Mod e1 e2) = show e1++"%"++show e2  
  show (If b e1 e2) = "if "++show b++" then "++show e1++" else "++show e2  
instance Show BExp where
  show (BoolLit b) = show b
  show (Not b) = "not "++show b
  show (And b1 b2) = "and "++show b1++" "++show b2
  show (Equal e1 e2) = show e1++"=="++show e2
  show (Greater e1 e2) = show e1++">"++show e2  
