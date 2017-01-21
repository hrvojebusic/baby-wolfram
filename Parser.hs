module Parser
( parse
) where

import ProjectData
import Lexer

-- Traverses input tokens, performs conversion of tokenized expression from
-- infix to prefix notation and applies step function, which builds expression
-- tree. Error is raised if there are no tokens given for parsing.
parse :: [Token] -> Tree
parse [] = error "No tokens were given for parsing"
parse ts = head $ foldl step [] (rpnCalc ts)

-- Groups tokens in tree nodes and constructs expression tree.
step :: [Tree] -> Token -> [Tree]
step [] (TokNumb n)       = [NumNode n]
step t (TokNumb n)        = NumNode n : t
step t (TokIden xs)       = VarNode xs : t
step (r:l:t) (TokOper op) = BiOpNode op l r : t
step (a:t) (TokFunc fun)  = UnOpNode fun a : t

-- Performs conversion of tokenized expression from infix to prefix notation.
rpnCalc :: [Token] -> [Token]
rpnCalc [] = []
rpnCalc tokens = rpnCalc' [] tokens
  where
    rpnCalc' :: [Token] -> [Token] -> [Token]
    rpnCalc' stack [] = clearStack stack
    rpnCalc' stack (t:ts) = case t of
        TokNumb _ -> t : rpnCalc' stack ts
        TokIden _ -> t : rpnCalc' stack ts
        TokFunc _ -> rpnCalc' (t:stack) ts
        TokOper _ ->
          let (nS, nQ) = opEn t stack
          in nQ ++ rpnCalc' nS ts
        TokLPar -> rpnCalc' (t:stack) ts
        TokRPar ->
          let (nS, nQ) = rpEn stack
          in nQ ++ rpnCalc' nS ts

-- Function called when an operator is encountered.
-- Input ordering:
-- token -> current stack -> (new stack, tokens to be added to output)
opEn :: Token -> [Token] -> ([Token],[Token])
opEn t [] = ([t],[])
opEn t@(TokOper o1) s@(x:xs) = case x of
        TokOper o2 ->
          if (leftAssociativity o1 && precedence o1 <= precedence o2)
            || (not (leftAssociativity o1) && precedence o1 < precedence o2)
            then
              let (stack, queue) = opEn t xs
              in (stack, x : queue)
            else
              (t : s, [])
        _ -> (t : s, [])
opEn t stack = (t : stack,[])

-- Function called when right parenthesis is encountered.
-- Input ordering:
-- current stack -> (new stack, tokens to be added to output)
rpEn :: [Token] -> ([Token],[Token])
rpEn ts = if not (null stack)
            then
              case head stack of
                TokFunc _ -> (tail stack, queue ++ [head stack])
                _         -> t
            else t
  where
    rpEn' :: [Token] -> ([Token],[Token])
    rpEn' [] = ([],[])
    rpEn' (x:xs) = case x of
        TokLPar -> (xs, [])
        _ -> let (newStack, newQueue) = rpEn' xs
             in (newStack, x : newQueue)
    t@(stack, queue) = rpEn' ts

-- Function called when there is no more input tokens to consume and
-- stack needs to be cleared.
clearStack :: [Token] -> [Token]
clearStack [] = []
clearStack (t:ts) = case t of
            TokLPar -> error "Left parenthesis mismathced"
            TokRPar -> error "Right parenthesis mismathced"
            _       -> t : clearStack ts
