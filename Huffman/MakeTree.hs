module MakeTree (makeTree,mTree)  -- [(Char,Int)] -> Tree 
where 
import Types 
import Test.QuickCheck 

-- *** My makeTree wins
makeTree = foldr1 pair 
 
pair :: Tree -> Tree -> Tree 
pair t1 t2 = Node (v1+v2) t1 t2
  where 
  v1 = value t1
  v2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _) = n    



-- original makeTree definition 
mTree :: [Tree] -> Tree
mTree [t] = t
mTree ts = makeTree (amalgamate ts) 

insTree :: Tree -> [Tree] -> [Tree]
insTree t [] = [t]
insTree t (t1:ts) 
  | (value t <= value t1) = t:t1:ts
  | otherwise             = t1 : insTree t ts

amalgamate :: [ Tree ] -> [ Tree ]
amalgamate ( t1 : t2 : ts )
  = insTree (pair t1 t2) ts
