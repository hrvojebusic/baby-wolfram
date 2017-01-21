module Lexer
( tokenize
) where

import ProjectData
import Data.Char

-- Performs input tokenization.
tokenize :: String -> [Token]
tokenize = map functionize . tokenize'

-- Performs actual input tokenization. Whitespaces are ignored from input.
-- At this step in input tokenization, identifiers are not filtered for
-- keywords which can have different meaning and swaped for different
-- tokens down the road.
tokenize' :: String -> [Token]
tokenize' [] = []
tokenize' (c : cs)
    | c `elem` supportedOperators = TokOper (operator c) : tokenize' cs
    | c == '('  = TokLPar : tokenize' cs
    | c == ')'  = TokRPar : tokenize' cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize' cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

-- Constructs identifier tokens.
identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokIden (c:str) : tokenize' cs'

-- Constructs number tokens.
number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = span isDigit cs in
   TokNumb (read (c : digs)) : tokenize' cs'

-- Checks whether identifier token can be interpreted as function.
-- Identifier tokens which represent a function are swaped for function
-- counterpart.
functionize :: Token -> Token
functionize t@(TokIden xs)
 | xs `elem` supportedFunctions = TokFunc (function xs)
 | otherwise = t
functionize t = t
