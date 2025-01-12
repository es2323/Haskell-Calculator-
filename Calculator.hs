{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isDigit" #-}

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
eval (OpNode '%' lhs rhs) =                      -- Evaluate modulus
    let lhsInt = round (eval lhs) :: Int 
        rhsInt = round (eval rhs) :: Int
    in fromIntegral (lhsInt `mod` rhsInt)  -- Modulus after converting to Int


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

-- Parse multiplication, division, and modulus 
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
        ('%':rhs) -> 
            let (rhsTree, rest') = parseMulDiv rhs
            in (OpNode '%' lhs rhsTree, rest')
        
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
parseFactor ('(':rest) =
    let (parsedExpr, rest') = parseAddSub rest  -- Parse inside parentheses recursively
    in case rest' of
        (')':rest'') -> (parsedExpr, rest'')  -- Closing parenthesis
        _           -> error "Mismatched parentheses"
parseFactor ('-':rest) =
    let (parsedExpr, rest') = parseFactor rest
    in (OpNode '-' (NumNode 0) parsedExpr, rest')  -- Handle unary minus
parseFactor input = parseNum input  -- Parse a number directly
-- Parse a number
parseNum :: String -> (Tree, String)
parseNum input =
    let (num, rest) = spanValidNum input  -- Get the number part
    in case reads num :: [(Double, String)] of
            [(n, "")] -> (NumNode n, rest)
            _         -> error ("Invalid number format: " ++ num)
            
-- Custom span function for valid number characters
spanValidNum :: String -> (String, String)
spanValidNum [] = ("", "")
spanValidNum (x:xs)
    | isDigit x || x == '.' = let (num, rest) = spanValidNum xs in (x:num, rest)
    | otherwise = ("", x:xs)

-- Check if a character is a digit
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Safely parse and evaluate an expression
safeEval :: String -> Either String Double
safeEval input =
    case parseSafe input of
        Left err -> Left err
        Right tree -> Right (eval tree)

-- Safely parse an expression
parseSafe :: String -> Either String Tree
parseSafe input =
    case parseAddSub input of
        (tree, []) -> Right tree
        (_, rest) -> Left ("Unexpected input: " ++ rest)

-- Main function for testing
main :: IO ()
main = do
    let expressions =
            [ "(2+4)*3"
            , "2*(3+4)-5"
            , "(3+5)*(7-4)"
            , "2*((3+5)*(7-4))"
            , "((2+3)*(4+1))+7"
            , "((8/2)+(3^2))*2"
            , "3^2+(5*4)-8/2"
            , "(2+3)*(4/2)+(6-1)"
            , "5*(3+(2^3))-6"
            , "10-(3+2)*4"
            , "4^(2+1)-7"
            , "((3+7)*(2-5))/2"
            , "10%3"        -- Modulus test
            , "10%(3-1)"    -- Modulus with parentheses test
            , "(3+7)*(2-"  -- Mismatched parentheses test
            , "2**"         -- Invalid number format test
            ]
    mapM_ (print . eval . fst . parseAddSub) expressions
