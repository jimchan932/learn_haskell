module Coding (codeMessage, decodeMessage) where
import Types   

codeMessage :: Table -> [Char] -> HCode
codeMessage tbl = concatMap (lookupTable tbl) 

lookupTable :: Table -> Char -> HCode
lookupTable [] c = error "lookupTable"
lookupTable ((ch,n):tb) c 
  | ch==c     = n
  | otherwise = lookupTable tb c
  
decodeMessage :: Tree -> HCode -> [Char]
decodeMessage tr = decodeByt tr
  where 
  decodeByt (Node n t1 t2) (L:rest) = decodeByt t1 rest
  decodeByt (Node n t1 t2) (R:rest) = decodeByt t2 rest
  decodeByt (Leaf c n) rest = c:decodeByt tr rest
  decodeByt t [] = []

abc = decodeMessage (Node 0 (Leaf 'a' 0) (Node 0 (Leaf 'b' 0) (Leaf 'c' 0)))  [L,R,L,R,R]

exam1 = Node 0 (Leaf 'a' 0) (Node 0 (Leaf 'b' 0) (Leaf 't' 0))

mess1 = [R,L,L,R,R,R,R,L,R,R]

battat = decodeMessage exam1 mess1

table1 = [('a',[L]),('b',[R,L]),('t',[R,R])]

code1 = codeMessage table1 battat 

