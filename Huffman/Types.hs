module Types (Tree(Leaf,Node), Bit(L,R), HCode, Table) where 

-- Trees to represent the relative frequencies of characters 
-- and therefore the Huffman codes

data Tree = Leaf Char Int | Node Int Tree Tree deriving (Eq,Show)

data Bit = L | R deriving (Eq,Show)
type HCode = [Bit]

type Table = [(Char,HCode)]
