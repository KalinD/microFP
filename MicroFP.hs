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
                  FunDef "f     ibonacci" [V "n"] (Add (FunCal "fibonacci" [(Sub (Id "n") (Val 1))]) (FunCal "fibonacci" [(Sub (Id "n") (Val 2))]))])

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
-- will generate a string that represents the EDSL program

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

prettyWithSep' :: [Param] -> String -> String
prettyWithSep' (e:[]) _ = pretty e
prettyWithSep' (e:es) sep = pretty e ++ sep ++ prettyWithSep' es sep

instance Pretty FunDef where
    pretty (FunDef fun args exp) = fun ++ " " ++ prettyWithSep' args " " ++ " = " ++ pretty exp
    -- pretty (FunDef fun args exp) = fun ++ " " ++ (map () args) ++ " = " ++ pretty exp

instance Pretty Param where
    pretty (V v) = v
    pretty (I i) = show i

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
    pretty (IfExpr comp exp1 exp2) = "if(" ++ (pretty comp) ++ ") {\n" ++ pretty exp1 ++ "\n}\nelse {\n" ++ pretty exp2 ++ "\n}"
    
     
-- FP3.4
-- Evaluates a Prog given in MicroFP EDSL format. The function, given as a string,
-- is evaluated with a list of integers to return a single integer result
class Bind a where
    bind :: a -> [Param] -> [Integer] -> a

instance Bind Expr where
    bind (Id v) params values = case elemIndex (V v) params of
        (Just a) -> (Val (values!!a))
        Nothing  -> (Val 0) -- Not found
--     bind (Val a) _ _ = (Val a)
--     bind (Add exp1 exp2) variables values = (Add (bind exp1 variables values) (bind exp2 variables values))
--     bind (Sub exp1 exp2) variables values = (Sub (bind exp1 variables values) (bind exp2 variables values))
--     bind (Mult exp1 exp2) variables values = (Mult (bind exp1 variables values) (bind exp2 variables values))
--     bind (FunCal name exprs) variables values = (FunCal name (fmap (\x -> bind x variables values) exprs))
--     bind (IfExpr compare exp1 exp2) variables values = (IfExpr (bind compare variables values) (bind exp1 variables values) (bind exp2 variables values))

-- instance Bind Compare where
--     bind (Smaller exp1 exp2) variables values = (Smaller (bind exp1 variables values) (bind exp2 variables values))
--     bind (Bigger exp1 exp2) variables values = (Bigger (bind exp1 variables values) (bind exp2 variables values))
--     bind (Equals exp1 exp2) variables values = (Equals (bind exp1 variables values) (bind exp2 variables values))

-- instance Bind FunDef where
--     bind (FunDef fun args exp) variables values = (FunDef fun args (bind exp variables values))

-- instance Bind Prog where
--     bind (Actions funcs) variables values = (Actions (map (\x -> bind x variables values) funcs)) 

-- instance Bind Param where
--     bind (V v) variables values = (V v)
--     bind (I i) variables values = (I i)
    
eval :: Prog -> String -> [Integer] -> Integer
eval (Actions []) _ _ = -69 -- Error
eval (Actions ((FunDef fname params expr):fs)) fcall values | fname == fcall = evalFunc (FunDef fname params expr) values
                                                            | otherwise      = eval (Actions fs) fcall values

evalExpr :: Expr -> FunDef -> [Param] -> [Integer] -> Integer
evalExpr (Id v) _ params values = value
    where
        (Val value) = bind (Id v) params values
evalExpr (Val value) _ _ _ = value
evalExpr (Add e1 e2) f params values = v1 + v2
    where
        v1 = (evalExpr e1 f params values)
        v2 = (evalExpr e2 f params values)
evalExpr (Sub e1 e2) f params values = v1 - v2
    where
        v1 = (evalExpr e1 f params values)
        v2 = (evalExpr e2 f params values)
evalExpr (Mult e1 e2) f params values = v1 * v2
    where
        v1 = (evalExpr e1 f params values)
        v2 = (evalExpr e2 f params values)
evalExpr (FunCal str es) f params values = (evalFunc f (fmap (\x -> evalExpr x f params values) es))
evalExpr (IfExpr compare e1 e2) f params values | evalCompare compare f params values = evalExpr e1 f params values
                                                | otherwise                           = evalExpr e2 f params values

evalCompare :: Compare -> FunDef -> [Param] -> [Integer] -> Bool
evalCompare (Smaller e1 e2) f params values = v1 < v2
    where
        v1 = (evalExpr e1 f params values)
        v2 = (evalExpr e2 f params values)
evalCompare (Bigger e1 e2) f params values = v1 > v2
    where
        v1 = (evalExpr e1 f params values)
        v2 = (evalExpr e2 f params values)
evalCompare (Equals e1 e2) f params values = v1 == v2
    where
        v1 = (evalExpr e1 f params values)
        v2 = (evalExpr e2 f params values) 

evalFunc :: FunDef -> [Integer] -> Integer
evalFunc (FunDef name params expr) values = evalExpr expr (FunDef name params expr) params values 

-- fibonacci 0 := 0;
-- fibonacci 1 := 1;
-- fibonacci n := fibonacci (n-1) + fibonacci (n-2);
-- eval microFibonacci "microFibonacci" [10]
-- [n] = [10]

-- FP4.1
-- Parsers for each of the types in our MicroFP EDSL definition
parseProgram :: Parser Prog
parseProgram = (Actions <$> (some parseFunction))

parseFunction :: Parser FunDef
parseFunction = (FunDef <$> identifier <*> (many parseParam) <*> (between (symbol ":=") parseExpr (symbol ";")))

parseExpr :: Parser Expr
parseExpr =  (Add <$> parseTerm <*> (symbol "+" *> parseExpr))
         <|> (Sub <$> parseTerm <*> (symbol "-" *> parseExpr))
         <|> parseTerm

parseParam :: Parser Param
parseParam =  (V <$> identifier)
          <|> (I <$> integer)

parseTerm :: Parser Expr
parseTerm =  (Mult <$> parseFactor <*> (symbol "*" *> parseTerm))
         <|> parseFactor

parseFactor :: Parser Expr
parseFactor =  (Val <$> integer)
           <|> (FunCal <$> identifier <*> parens (some parseExpr))
           <|> (Id <$> identifier)
           <|> (IfExpr <$> (symbol "if" *> parens (parseCompare)) <*> (between (symbol "then") (braces parseExpr) (symbol "else")) <*> braces parseExpr)
           <|> parens parseExpr

parseCompare :: Parser Compare
parseCompare =  (Bigger <$> (parseExpr <* symbol ">") <*> parseExpr)
            <|> (Smaller <$> (parseExpr <* symbol "<") <*> parseExpr)
            <|> (Equals <$> (parseExpr <* symbol "==") <*> parseExpr)

-- FP4.2
-- Parses a program written as a string and translates it to the MicroFP EDSL
compile :: String -> Prog
compile input = fst (head (runParser parseProgram (Stream input)))

-- FP4.3
-- Reads a file containing a program, compiles it with FP4.2, and evaluates it with FP3.4 and a
-- list of integers to return a single integer result. The last function is used if there is
-- more than one function in the file

-- runFile :: FilePath -> [Integer] -> IO Integer
-- runFile filepath vals = 
--     where
--         progs = compile <$> (lines <$> readFile filepath)


-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
