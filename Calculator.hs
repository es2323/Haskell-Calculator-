data Tree = NumNode Double         -- Leaf node for numbers
          | OpNode Char Tree Tree  -- Operator node with two subtrees
          deriving (Show)
dddfdfd