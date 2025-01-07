data Tree = NumNode Double         -- Leaf node for numbers
          | OpNode Char Tree Tree  -- Operator node with two subtrees
          deriving (Show)

-- Parser for basic expressions like "2+3"
parse :: String -> Tree
parse input = 
    let (lhs, op:rhs) = break (`elem` "+-*/^") input  -- Split at the operator
    in OpNode op (NumNode (read lhs)) (NumNode (read rhs))

main :: IO ()
main = do
    let expression1 = "2+3"
    let expression2 = "4*5"
    print (parse expression1)
    print (parse expression2)
