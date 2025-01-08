data Tree = NumNode Double         -- Leaf node for numbers
          | OpNode Char Tree Tree  -- Operator node with two subtrees
          deriving (Show)

-- Parser for basic expressions with operator precedence
parse :: String -> Tree
parse = parseAddSub   -- Start with the lowest precedence level

-- Evaluator for the parse tree
eval :: Tree -> Double
eval (NumNode n) = n  -- Base case: return the number for a leaf node
eval (OpNode '+' lhs rhs) = eval lhs + eval rhs  -- Evaluate and add
eval (OpNode '-' lhs rhs) = eval lhs - eval rhs  -- Evaluate and subtract
eval (OpNode '*' lhs rhs) = eval lhs * eval rhs  -- Evaluate and multiply
eval (OpNode '/' lhs rhs) = eval lhs / eval rhs  -- Evaluate and divide
eval (OpNode '^' lhs rhs) = eval lhs ** eval rhs -- Evaluate exponentiation

-- Parse addition and subtraction
parseAddSub :: String -> Tree
parseAddSub input =
    let (lhs, rest) = parseMulDiv input  -- Parse the left-hand side
    in case rest of
        ('+':rhs) -> OpNode '+' lhs (parseAddSub rhs)  -- Parse the rest recursively
        ('-':rhs) -> OpNode '-' lhs (parseAddSub rhs)  -- Handle subtraction
        _         -> lhs                              -- No more operators at this level

-- Parse multiplication and division
parseMulDiv :: String -> (Tree, String)
parseMulDiv input =
    let (lhs, rest) = parseExp input  -- Parse the left-hand side
    in case rest of
        ('*':rhs) -> let (rhsTree, rest') = parseMulDiv rhs
                     in (OpNode '*' lhs rhsTree, rest')
        ('/':rhs) -> let (rhsTree, rest') = parseMulDiv rhs
                     in (OpNode '/' lhs rhsTree, rest')
        _         -> (lhs, rest)  -- No more operators at this level

-- Parse exponentiation
parseExp :: String -> (Tree, String)
parseExp input =
    let (lhs, rest) = parseNum input  -- Parse the left-hand side
    in case rest of
        ('^':rhs) -> let (rhsTree, rest') = parseExp rhs
                     in (OpNode '^' lhs rhsTree, rest')
        _         -> (lhs, rest)  -- No more operators at this level

-- Parse a number
parseNum :: String -> (Tree, String)
parseNum input =
    let (num, rest) = span (`elem` "0123456789") input  -- Extract the number
    in (NumNode (read num), rest)

main :: IO ()
main = do
    let expression1 = "2+4*3"        -- 2 + (4 * 3) = 14
    let expression2 = "3^2+4*2"     -- (3^2) + (4 * 2) = 9 + 8 = 17
    let expression3 = "10-2/2"      -- 10 - (2 / 2) = 10 - 1 = 9
    print $ eval (parse expression1)  -- Should print 14
    print $ eval (parse expression2)  -- Should print 17
    print $ eval (parse expression3)  -- Should print 9
