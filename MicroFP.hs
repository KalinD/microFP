-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Kalin Doychev (s2535645)
-- Student 2: Tariq Riahi (s2619393)
-- Student 3: Vincent Albertsson (s2247259)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All

-- Additional imports
import Data.List
import Debug.Trace

-- FP3.1
-- Expression type with definitions for addition (+), subtraction (-),
-- multiplication (*), const value (identifier or number), function call,
-- and an "if" statement
exprEx = (Add (Val 1) (Val 2))
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Val Integer
          | Id String
          | FunCal String [Expr]
          | IfExpr Compare Expr Expr
          deriving Show

-- Compare type to handle comparison in the "if" statement. Can handle 
-- less than (<), larger than (>), and equals (==) comparisons.
compareEx = (Equals (Val 1) (Val 1))
data Compare = Smaller Expr Expr
             | Bigger Expr Expr
             | Equals Expr Expr
             deriving Show

-- Program type containing the main program with a list of functions.
progEx = (Actions [FunDef "functionEx" [V "x"] (Add (Id "x") (Val 1))])
data Prog = Actions [FunDef]
          deriving Show

-- Function definition type. Contains the definition of the function,
-- with its name, parameters and the definition iteslef (expression).
funEx = (FunDef "functionEx" [V "x"] (Add (Id "x") (Val 1)))
data FunDef = FunDef String [Param] Expr -- TODO: Fix the second argument
            deriving Show

-- Parameters type - the parameters of the function can be either an identifier or an integer.
paramEx = (V "var")
data Param = V String
           | I Integer
           deriving (Show, Eq)

-- FP3.2
-- Example functions taken from "functions.txt" and written in the form of the MicroFP EDSL defined in FP 3.1
microFibonacci :: Prog
microFibonacci = (Actions [FunDef "fibonacci" [I 0] (Val 0),
                  FunDef "fibonacci" [I 1] (Val 1),
                  FunDef "fibonacci" [V "n"] (Add (FunCal "fibonacci" [(Sub (Id "n") (Val 1))]) (FunCal "fibonacci" [(Sub (Id "n") (Val 2))]))])

microFib :: Prog
microFib = (Actions [FunDef "fib" [V "n"] 
                (IfExpr (Smaller (Id "n") (Val 3)) (Val 1) 
                (Add (FunCal "fib" [(Sub (Id "n") (Val 1))]) (FunCal "fib" [(Sub (Id "n") (Val 2))]))
                )])

microSum :: Prog
microSum = (Actions [FunDef "sum" [I 0] (Val 0),
                     FunDef "sum" [V "a"] (Add (FunCal "sum" [(Sub (Id "a") (Val 1))]) (Id "a"))])   

microDiv :: Prog
microDiv = (Actions [FunDef "div" [(V "x"), (V "y")] 
            (IfExpr (Smaller (Id "x") (Id "y")) (Val 0) (Add (Val 1) 
            (FunCal "div" [(Sub (Id "x") (Id "y")), (Id "y")])))])

microTwice :: Prog
microTwice = (Actions [FunDef "twice" [(V "f"), (V "x")] (FunCal "twice" [(FunCal "twice" [(Id "x")])])])

microAdd :: Prog
microAdd = (Actions [FunDef "add" [(V "x"), (V "y")] (Add (Id "x") (Id "y"))])

microInc :: Prog
microInc = (Actions [FunDef "inc" [] (FunCal "add" [(Val 1)])])

microEleven :: Prog
microEleven = (Actions [FunDef "eleven" [] (FunCal "inc" [(Val 10)])])

-- FP3.3
-- Given a program written in the form of the MicroFP EDSL, the pretty function
-- will generate a string that represents the EDSL program in a easier to read format.
prettyEx = putStr (pretty microFib)
class Pretty a where
    pretty :: a -> String

instance Pretty Prog where
    pretty (Actions []) = ""
    pretty (Actions (f:fs)) = pretty f ++ "\n" ++ pretty (Actions fs)

instance Pretty Compare where
    pretty (Smaller exp1 exp2) = pretty exp1 ++ " < " ++ pretty exp2
    pretty (Bigger exp1 exp2) = pretty exp1 ++ " > " ++ pretty exp2
    pretty (Equals exp1 exp2) = pretty exp1 ++ " == " ++ pretty exp2

-- Helper function to display parameters of a function with a specific separator 
prettyWithSep' :: [Param] -> String -> String
prettyWithSep' [] _ = ""
prettyWithSep' (e:[]) _ = pretty e
prettyWithSep' (e:es) sep = pretty e ++ sep ++ prettyWithSep' es sep

instance Pretty FunDef where
    pretty (FunDef fun args exp) = fun ++ " " ++ prettyWithSep' args " " ++ " := " ++ pretty exp ++ ";"

instance Pretty Param where
    pretty (V v) = v
    pretty (I i) = show i

-- Helper function to display expressions with a specific separator. Really useful 
-- to print function calls with multiple arguments.
prettyWithSep :: [Expr] -> String -> String
prettyWithSep (e:[]) _ = pretty e
prettyWithSep (e:es) sep = pretty e ++ sep ++ prettyWithSep es sep

instance Pretty Expr where
    pretty (Add exp1 exp2) = pretty exp1 ++ " + " ++ pretty exp2
    pretty (Sub exp1 exp2) = pretty exp1 ++ " - " ++ pretty exp2
    pretty (Mult exp1 exp2) = pretty exp1 ++ " * " ++  pretty exp2
    pretty (Val v) = show v
    pretty (Id v) = v
    pretty (FunCal name es) = name ++ "(" ++ (prettyWithSep es ", ") ++ ")"
    pretty (IfExpr comp exp1 exp2) = "if(" ++ (pretty comp) ++ ") then {\n" ++ pretty exp1 ++ "\n}\nelse {\n" ++ pretty exp2 ++ "\n}"
    
     
-- FP3.4 with FP5.2 and FP5.4
-- Helps tansform variables into values for easier calculations.
bind :: Expr -> [Param] -> [Integer] -> Expr
bind (Id v) params values = case elemIndex (V v) params of
    (Just a) -> (Val (values!!a))
    Nothing  -> error "Value not found"

-- Evaluates a Prog given in MicroFP EDSL format. The function, given as a string,
-- is evaluated with a list of integers to return a single integer result
evalEx1 = eval microFib "fib" [10]
evalEx2 = eval microDiv "div" [999, 2]
eval :: Prog -> String -> [Integer] -> Integer
eval (Actions []) _ _ = error "No functions found" -- Error
eval prog fname values
        | (length nameAndValueMatch) > 0 = evalFunc (head nameAndValueMatch) prog values
        | otherwise                      = evalFunc lastNameMatch prog values
    where
        functions = getFunctions prog
        nameFiltered = filter (\(FunDef name params expr) -> name == fname) functions
        nameAndValueMatch = filter (\(FunDef name params expr) -> (I <$> values) == params) nameFiltered
        lastNameMatch = last nameFiltered

-- Evaluates expressions to their actual integer value
evalExpr :: Expr -> Prog -> [Param] -> [Integer] -> Integer
evalExpr (Id v) _ ps vs = value
    where (Val value) = bind (Id v) ps vs
evalExpr (Val value) _ _ _ = value
evalExpr (Add e1 e2) p ps vs = v1 + v2
    where v1 = (evalExpr e1 p ps vs)
          v2 = (evalExpr e2 p ps vs)
evalExpr (Sub e1 e2) p ps vs = v1 - v2
    where v1 = (evalExpr e1 p ps vs)
          v2 = (evalExpr e2 p ps vs)
evalExpr (Mult e1 e2) p ps vs = v1 * v2
    where v1 = (evalExpr e1 p ps vs)
          v2 = (evalExpr e2 p ps vs)
evalExpr (FunCal fname es) p ps vs | argsCount == paramsCount = (eval p fname (fmap (\x -> evalExpr x p ps vs) es))
                                   | otherwise                = (eval p fname (fmap (\x -> evalExpr x p ps vs) (es++newArgs)))
    where
        argsCount = getNumberOfArguments p fname
        paramsCount = length es
        newArgs = (fmap (\x -> Val x) vs)
evalExpr (IfExpr compare e1 e2) p ps vs | evalCompare compare p ps vs = evalExpr e1 p ps vs
                                        | otherwise                   = evalExpr e2 p ps vs

-- Helps get the number of arguments a function definition has for a given program. 
getNumberOfArguments :: Prog -> String -> Int
getNumberOfArguments prog fname = length args
    where
        functions = getFunctions prog
        (FunDef _ args _) = head (filter (\(FunDef name params expr) -> name == fname) functions)

-- Evaluates a compare data type and turns it into a boolean.
evalCompare :: Compare -> Prog -> [Param] -> [Integer] -> Bool
evalCompare (Smaller e1 e2) p ps vs = v1 < v2
    where
        v1 = (evalExpr e1 p ps vs)
        v2 = (evalExpr e2 p ps vs)
evalCompare (Bigger e1 e2) p ps vs = v1 > v2
    where
        v1 = (evalExpr e1 p ps vs)
        v2 = (evalExpr e2 p ps vs)
evalCompare (Equals e1 e2) p ps vs = v1 == v2
    where
        v1 = (evalExpr e1 p ps vs)
        v2 = (evalExpr e2 p ps vs) 

-- Evaluate a function definition by ecaluating its definition.
evalFunc :: FunDef -> Prog -> [Integer] -> Integer
evalFunc (FunDef name ps e) p vs = evalExpr e p ps vs 

-- FP4.1
-- Parsers for each of the types in our MicroFP EDSL definition
parseProgEx = runParser parseProgram (Stream "add x y := x + y;")
parseProgram :: Parser Prog
parseProgram = (Actions <$> (some parseFunction))

parseFunction :: Parser FunDef
parseFunction = (FunDef <$> identifier <*> (many parseParam) <*> (between (symbol ":=") parseExpr (symbol ";")))

parseExpr :: Parser Expr
parseExpr =  (Add <$> whitespace parseTerm <*> (symbol "+" *> whitespace parseExpr))
         <|> (Sub <$> whitespace parseTerm <*> (symbol "-" *> whitespace parseExpr))
         <|> whitespace parseTerm

parseParam :: Parser Param
parseParam =  (V <$> identifier)
          <|> (I <$> integer)

parseTerm :: Parser Expr
parseTerm =  (Mult <$> parseFactor <*> (symbol "*" *> parseTerm))
         <|> parseFactor

parseFactor :: Parser Expr
parseFactor =  (Val <$> integer)
           <|> (IfExpr <$> (symbol "if" *> parens (parseCompare)) <*> (between (symbol "then") (braces parseExpr) (symbol "else")) <*> braces parseExpr)
           <|> (FunCal <$> identifier <*> parens (sep1 parseExpr (char ',')))
           <|> (Id <$> identifier)
           <|> parens parseExpr

parseCompare :: Parser Compare
parseCompare =  (Bigger <$> (parseExpr <* symbol ">") <*> parseExpr)
            <|> (Smaller <$> (parseExpr <* symbol "<") <*> parseExpr)
            <|> (Equals <$> (parseExpr <* symbol "==") <*> parseExpr)

-- Pattern matching function that combines multiple definitions of the
-- same function into a single one by using "if" statements.
patmatch :: Prog -> Prog
patmatch p = (Actions result)
    where
        functions = getFunctions p
        groupedByName = groupBy (\(FunDef name1 _ _) (FunDef name2 _ _) -> name1 == name2) functions
        midResult = fmap combineFunctions groupedByName
        lastOfEach = fmap last groupedByName
        lastNameOfEach = fmap (\(FunDef name _ _) -> name) lastOfEach
        lastArgOfEach = fmap (\(FunDef _ args _) -> args) lastOfEach
        result = zipWith3 (\name args expr -> (FunDef name args expr))  lastNameOfEach lastArgOfEach midResult

-- Helper function to actually combine the function definitions into a single expression.
combineFunctions :: [FunDef] -> Expr
combineFunctions ((FunDef _ _ e):[]) = e
combineFunctions ((FunDef name args exp):fs) =
    (IfExpr (Equals (Id argName) (Val val)) exp (combineFunctions fs))  
    where
        (FunDef _ argsLast _) = last fs
        (V argName) = head argsLast 
        (I val) = args!!0

-- FP4.2
-- Parses a program written as a string and translates it to the MicroFP EDSL
compileEx = compile "double a := a*2;"
compile :: String -> Prog
compile input = patmatch (fst (head (runParser parseProgram (Stream input))))

-- FP4.3
{- Reads a file containing a program, compiles it with FP4.2, and finally evaluates it with 
 - FP3.4 and a list of integers to return a single integer result. The last function is used 
 - if there is more than one function in the file
 -}
getFunctions :: Prog -> [FunDef]
getFunctions (Actions funs) = funs

-- Returns the name of the functions
getFunctionName :: FunDef -> String
getFunctionName (FunDef fname _ _) = fname

{- fib.txt consists of:
fibonacci 0 := 0;
fibonacci 1 := 1;
fibonacci n := fibonacci (n-1) + fibonacci (n-2);

fib n := if (n < 3) then {
           1
 	 } else {
	   fib (n-1) + fib (n-2)
  	 };
-}
{- The runFile function compiles the file to an Prog with many FunDef. Then it executes 
 - the last one with the parameters given in vals ([Integer]) and returns the result.
-}
runFileEx = runFile "./fib.txt" [10]
runFile :: FilePath -> [Integer] -> IO Integer
runFile filepath vals = (\x y -> eval x y vals) <$> prog <*> fname
    where 
        prog = compile <$> (concat <$> (lines <$> readFile filepath))
        fname = getFunctionName <$> (last <$> (getFunctions <$> prog))

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
