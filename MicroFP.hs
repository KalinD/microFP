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

-- FP 3.1
data Expr = Add Expr Expr
          | Sub Expr Expr    -- Maybe convert to Add
          | Mult Expr Expr
          | Val Integer
          | Id String
          | FunCal String [Expr]
          | IfExpr Compare Expr Expr
          deriving Show

data Compare = Smaller Expr Expr
             | Bigger Expr Expr
             | Equals Expr Expr
             deriving Show

data Prog = Actions [FunDef]
          deriving Show

data FunDef = FunDef String [Param] Expr -- TODO: Fix the second argument
            deriving Show

data Param = V String
           | I Integer
           deriving Show


-- FP3.2
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

-- microDouble :: Prog
-- microDouble = (Actions [FunDef "double" [(Id "a")] (Mult (Id "a") (Val 2))])

-- microForty :: Prog
-- microForty = (Actions [FunDef "forty" [] (FunCal "twice" [(FunCal "Double" [(Val 10)])])])

-- microMain :: Prog
-- microMain = (Actions [FunDef "main" [] (FunCal "div" [(Val 999),(Val 2)])])

-- FP 3.3
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
-- eval :: Prog -> String -> [Integer] -> Integer

myElemIndex :: (Eq a) => a -> [a] -> Maybe Int
myElemIndex e es = myElemIndex' e es 0

myElemIndex' :: (Eq a) => a -> [a] -> Int -> Maybe Int
myElemIndex' _ [] _ = Nothing
myElemIndex' e (x:es) i | e == x    = (Just i)
                        | otherwise = myElemIndex' e es (i + 1)


class Bind a where
    bind :: a -> [String] -> [Integer] -> a

instance Bind Expr where
    bind (Id v) variables values = case myElemIndex v variables of
        (Just a) -> (Val (toInteger a))
        Nothing  -> (Val 0) -- Not found
    bind (Val a) _ _ = (Val a)
    bind (Add exp1 exp2) variables values = (Add (bind exp1 variables values) (bind exp2 variables values))
    bind (Sub exp1 exp2) variables values = (Sub (bind exp1 variables values) (bind exp2 variables values))
    bind (Mult exp1 exp2) variables values = (Mult (bind exp1 variables values) (bind exp2 variables values))
    bind (FunCal name exprs) variables values = (FunCal name (fmap (\x -> bind x variables values) exprs))
    bind (IfExpr compare exp1 exp2) variables values = (IfExpr (bind compare variables values) (bind exp1 variables values) (bind exp2 variables values))

instance Bind Compare where
    bind (Smaller exp1 exp2) variables values = (Smaller (bind exp1 variables values) (bind exp2 variables values))
    bind (Bigger exp1 exp2) variables values = (Bigger (bind exp1 variables values) (bind exp2 variables values))
    bind (Equals exp1 exp2) variables values = (Equals (bind exp1 variables values) (bind exp2 variables values))

instance Bind FunDef where
    bind (FunDef fun args exp) variables values = (FunDef fun (map (\x -> bind x variables values) args) (bind exp variables values))

instance Bind Prog where
    bind (Actions funcs) variables values = (Actions (map (\x -> bind x variables values) funcs)) 

instance Bind Param where
    bind (V v) variables values = (V v)
    bind (I i) variables values = (I i)

-- FP4.1

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
-- compile :: String


-- FP4.3
-- runFile :: FilePath -> [Integer] -> IO Integer
-- runFile filepath vals = calculations n <$> (lines <$> readFile filepath)

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
