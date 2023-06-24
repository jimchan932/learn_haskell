module Queue2 (Queue,
               emptyQ,
               isEmptyQ,
			   addQ, remQ,
			   Deque, 
               inject,
               push,
               pop,		   
			   ) where 
import Test.QuickCheck
			   
data Queue a = Queue [a] [a] 

emptyQ :: Queue a
emptyQ = Queue [] []

isEmptyQ :: Eq a => Queue a -> Bool
isEmptyQ (Queue xs ys) = xs==[] && ys==[]

addQ :: a -> Queue a -> Queue a
addQ x (Queue xs ys) = Queue xs (x:ys)

remQ :: Queue a -> (a, Queue a)
remQ (Queue [] []) = error "remQ"
remQ (Queue [] ys) = remQ (Queue (reverse ys) []) 
remQ (Queue (x:xs) ys) = (x, Queue xs ys) 

prop_remQ :: Eq a => [a] -> [a] -> Bool
prop_remQ xs ys = 
    if xs==[] || ys==[] then True else last xs == fst (remQ (Queue xs ys))
  
data Deque a = Deque [a] [a] 

inject :: a -> Deque a -> Deque a 
inject x (Deque xs ys) = Deque (x:xs) ys 

push :: a -> Deque a -> Deque a 
push x (Deque xs ys) = Deque xs (x:ys)

eject :: Deque a -> (a,Deque a) 
eject (Deque [] []) = error "eject"
eject (Deque [] ys) = eject (Deque (reverse ys) [])
eject (Deque (x:xs) ys) = (x, Deque xs ys)

pop :: Deque a -> (a,Deque a)
pop (Deque [] []) = error "pop"
pop (Deque xs []) = pop (Deque [] (reverse xs))
pop (Deque xs (y:ys)) = (y,Deque xs ys) 


