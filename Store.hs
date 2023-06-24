module Store 
    ( Store, 
	  initial,  -- Store
	  value,    -- Store -> Var -> Integer
	  maybeVal,    -- Store -> Var -> Maybe Integer
	  update,   -- Store -> Var -> Integer -> Store 
          setAll    -- Integer -> Store         
    ) where 
import Test.QuickCheck	  
import Data.List (nub) 

type Var = Char 	  
newtype Store = Store [(Integer,Var)] 

init :: [(Integer,Var)]
init = []

val :: [(Integer,Var)] -> Var -> Integer
val [] v = 0
val ((n,w):sto) v 
  | w==v = n
  | True = val sto v 

upd :: [(Integer,Var)] -> Var -> Integer -> [(Integer,Var)]
upd sto v n = ((n,v):sto) 

initial :: Store
initial = Store []

value (Store ((n,w):sto)) v =
  if w==v then n
     else value (Store sto) v 

maybeVal :: Store -> Var -> Maybe Integer
maybeVal (Store []) _ = Nothing
maybeVal sto  v = Just (value sto v)

hasValue :: Store -> Var -> Bool
hasValue sto v = maybeVal sto v /= Nothing
  
update :: Store -> Var -> Integer -> Store
update (Store sto) v n = sort (Store ((n,v):sto))

sort :: Store -> Store 
sort (Store []) = Store []
sort (Store (x:xs)) = Store (nub (smaller ++ [x] ++ larger))
  where
  (Store smaller) = sort (Store [(n,c) |(n,c)<-xs, lesser (n,c) x])
  (Store larger)  = sort (Store [(n,c) |(n,c)<-xs, lesser x (n,c)])
  
lesser :: (Integer,Var) -> (Integer,Var) -> Bool  
lesser (n1,c1) (n2,c2) = c1<c2 || (c1==c2 && n1<n2)

setAll :: Integer -> Store
setAll n = Store (map (\v -> (n,v)) ['a'..'z'])

instance Eq Store where 
  (==) = (\sto1 sto2 -> sort sto1 == sort sto2)

instance Show Store where 
  showsPrec n (Store sto) = showsPrec n sto 

instance Arbitrary Store where
    arbitrary = do
      list <- listOf element
      return $ Store list
                where
                  element =
                      do
                        n <- arbitrary
                        v <- elements ['a'..'z']
                        return (n,v)   
