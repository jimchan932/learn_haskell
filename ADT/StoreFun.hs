module StoreFun
    ( Store, 
	  initial,  -- Store 
	  value,    -- Store -> Var -> Maybe Integer
	  update,   -- Store -> Var -> Maybe Integer -> Store 
	  hasValue, 
	  setAll
	  ) where 

type Var = Char
newtype Store = Store (Var -> Maybe Integer)

initial :: Store 
initial = Store (\_ -> Nothing)

value :: Store -> Var -> Maybe Integer 
value (Store f) = f 

hasValue sto v = value sto v /= Nothing

update :: Store -> Var -> Maybe Integer -> Store
update (Store sto) v n 
  = Store (\w -> if  v==w then n else sto v) 

setAll :: Maybe Integer -> Store 
setAll n = Store (\_ -> n)  
--isX = value (update initial 'x' 1)
 
instance Eq Store where 
  (Store sto1) == (Store sto2) = all (\ch -> sto1 ch==sto2 ch) ['a'..'z']
  
