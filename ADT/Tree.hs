module Tree 
  (Tree,
   nil,
   isNil,
   isNode,
   leftSub,
   rightSub,
   treeVal,
   insTree,
   delete,
   minTree
  ) where 
import Prelude hiding (succ)  

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show
 
insTree :: Ord a => a -> Tree a -> Tree a
insTree val Nil = (Node val Nil Nil)
insTree val (Node v t1 t2) 
  | val==v  = Node v t1 t2
  | val > v = Node v t1 (insTree val t2)
  | val < v = Node v (insTree val t1) t2
   
delete :: Ord a => a -> Tree a -> Tree a 
delete val (Node v t1 t2)
  | val < v   = Node v (delete val t1) t2
  | val > v   = Node v t1 (delete val t2)
  | isNil t2  = t1
  | isNil t1  = t2
  | otherwise = join t1 t2

minT, maxT :: Ord a => Tree a -> Maybe a
minT t 
  | isNil t  = Nothing
  | isNil t1 = Just v
  | otherwise = minT t1
    where 
	t1 = leftSub t 
	v  = treeVal t 
	
maxT t 
  | isNil t  = Nothing
  | isNil t2 = Just v
  | otherwise = maxT t2
    where 
	t2 = rightSub t 
	v  = treeVal t 
	
join :: Ord a => Tree a -> Tree a -> Tree a 	
join t1 t2
  = Node mini t1 newt
  where 
  (Just mini) = minTree t2
  newt        = delete mini t2
  
nil :: Tree a 
nil = Nil

isNil :: Tree a -> Bool
isNil Nil = True
isNil _   = False 

isNode :: Tree a -> Bool
isNode = not.isNil 

leftSub :: Tree a -> Tree a 
leftSub Nil           = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a 
rightSub Nil           = error "rightSub"
rightSub (Node _ _ t2) = t2

treeVal :: Tree a -> a
treeVal Nil          = error "treeVal"
treeVal (Node v _ _) = v

indexT n t
  | isNil t   = error "indexT"
  | n<st1     = indexT n t1 
  | n==st1    = v
  | otherwise = indexT (v-st1-1) t2  -- special
    where
	v = treeVal t
	t1 = leftSub t
	t2 = rightSub t
	st1 = size t1
	
size :: Tree a -> Int	
size t 
  | isNil t   = 0
  | otherwise = 1 + size (leftSub t) + size (rightSub t) 

{-closest :: Int -> Tree Int -> Int
closest n Nil = error "closest"
closest n t 
  | minTree t==n  = successor n t 
  | n<v           = closest 
  
    where 
    v = treeVal t 	-}


successor :: Ord a => a -> Tree a -> Maybe a 
successor _ Nil = Nothing 
successor val (Node v t1 t2)
  | (isNil t2 && val==v) || (isNil t1 && val<v) = Nothing 
  | val==v = minTree t2 
  | val==l = Just v
  | val<l  = successor val t1
  | val>v  = successor val t2   
    where 
	l = treeVal t1  

minTree, maxTree :: Ord a => Tree a -> Maybe a   
minTree = fst.treeVal.endTrees   
maxTree = snd.treeVal.endTrees     

endTree :: Ord a => Tree a -> Tree (Maybe a,Maybe a)
endTree Nil = Node (Nothing,Nothing) Nil Nil
endTree (Node v Nil Nil) = Node (Just v,Just v) Nil Nil
endTree (Node v t1 t2) 
  | isNil t1  = Node (Just v,v2) Nil (endTree t2)
  | isNil t2  = Node (v1,Just v) (endTree t1) Nil
  | otherwise = Node (v1,v2) (endTree t1) (endTree t2)
    where
    v1 = minTree t1
    v2 = maxTree t2	
				 
occurTree :: Ord a => a -> Tree a -> Tree a 
occurTree val (Node v t1 t2)
  | v==val = join (occurTree val t1) (occurTree val t2)
  | isNil t2 || val<v2 = occurTree val t1 
  | isNil t1 || val>v1 = occurTree val t2
    where
	v1 = treeVal t1
	v2 = treeVal t2
{-
searchTree (Node v t1 t2) 
  | v==v1 && v==v2 = searchTree (Node  
    where 
    v1 = treeVal t1
    v2 = treeVal t2  -}

  
