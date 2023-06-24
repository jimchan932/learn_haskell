module Queue1
  ( Queue,
    emptyQ,     -- Queue a 
	isEmptyQ,   -- Queue a -> Bool
	addQ,       -- a -> Queue a -> Queue a
	remQ     -- Queue a -> (a,Queue a)
   ) where
   
newtype Queue a = Queue [a]

emptyQ :: Queue a
emptyQ = Queue []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue []) = True
isEmtpyQ _          = False
  	
addQ :: a -> Queue a -> Queue a 
addQ x (Queue xs) = Queue (xs ++ [x])

remQ :: Queue a -> (a, Queue a)
remQ q@(Queue xs) =
     if isEmptyQ q then error "remQ" else (head xs, Queue (tail xs))

	 
	 



	