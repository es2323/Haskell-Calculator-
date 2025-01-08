-- Define the Tree data type
data Tree = NumNode Double         -- Leaf node for numbers
          | OpNode Char Tree Tree  -- Operator node with two subtrees
          deriving (Show)

-- Evaluator for the parse tree
eval :: Tree -> Double
eval (NumNode n) = n                              -- Base case: return the number for a leaf node
eval (OpNode '+' lhs rhs) = eval lhs + eval rhs  -- Evaluate and add
eval (OpNode '-' lhs rhs) = eval lhs - eval rhs  -- Evaluate and subtract
eval (OpNode '*' lhs rhs) = eval lhs * eval rhs  -- Evaluate and multiply
eval (OpNode '/' lhs rhs) = eval lhs / eval rhs  -- Evaluate and divide
eval (OpNode '^' lhs rhs) = eval lhs ** eval rhs -- Evaluate exponentiation

-- Parse addition and subtraction
parseAddSub :: String -> (Tree, String)
parseAddSub input =
    let (lhs, rest) = parseMulDiv input  -- Parse the left-hand side
    in case rest of
        ('+':rhs) -> 
            let (rhsTree, rest') = parseAddSub rhs
            in (OpNode '+' lhs rhsTree, rest')
        ('-':rhs) -> 
            let (rhsTree, rest') = parseAddSub rhs
            in (OpNode '-' lhs rhsTree, rest')
        _ -> (lhs, rest)  -- No more operators at this level

-- Parse multiplication and division
parseMulDiv :: String -> (Tree, String)
parseMulDiv input =
    let (lhs, rest) = parseExp input  -- Parse the left-hand side
    in case rest of
        ('*':rhs) -> 
            let (rhsTree, rest') = parseMulDiv rhs
            in (OpNode '*' lhs rhsTree, rest')
        ('/':rhs) -> 
            let (rhsTree, rest') = parseMulDiv rhs
            in (OpNode '/' lhs rhsTree, rest')
        _ -> (lhs, rest)  -- No more operators at this level

-- Parse exponentiation
parseExp :: String -> (Tree, String)
parseExp input =
    let (lhs, rest) = parseFactor input  -- Parse the left-hand side
    in case rest of
        ('^':rhs) -> 
            let (rhsTree, rest') = parseExp rhs
            in (OpNode '^' lhs rhsTree, rest')
        _ -> (lhs, rest)  -- No more operators at this level

-- Parse a factor (number, parenthesis, or negative sign)
parseFactor :: String -> (Tree, String)
parseFactor ('-':rest) =
    let (tree, rest') = parseFactor rest  -- Parse the rest of the factor as a negative number
    in (OpNode '-' (NumNode 0) tree, rest')  -- Represent "-x" as "0 - x"
parseFactor ('(':rest) =
    let (parsedExpr, rest') = parseAddSub rest  -- Parse inside parentheses recursively
    in case rest' of
        (')':rest'') -> (parsedExpr, rest'')   -- Closing parenthesis
        _            -> error "Mismatched parentheses"
parseFactor input =
    let (num, rest) = parseNum input           -- Parse a number directly
    in (num, rest)
-- Parse a number
parseNum :: String -> (Tree, String)
parseNum input =
    let (num, rest) = span (`elem` (['0'..'9'] ++ ".")) input  -- Get the number part (supports decimals)
    in if null num
        then error "Expected a number"
        else (NumNode (read num), rest)

-- Top-level parse function
parse :: String -> Tree
parse input =
    let (tree, rest) = parseAddSub input
    in if null rest
        then tree
        else error $ "Unexpected input remaining: " ++ rest

-- Main function to test expressions
main :: IO ()
main = do
    let expressions =
            [ "-5+3"
            , "2*(-3)"
            , "-(3+5)*2"
            , "4+(-2*3)"
            , "3+(-4)^2"
            , "-(2+(3*(-4)))"
            , "2^(-3)"
            , "((3-7)*(-2))/2"
            ]
    mapM_ (print . eval . parse) expressions
