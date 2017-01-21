module BabyWolfram
( createExpression
, substitute
, evaluate
, deriveBy
)where

import ProjectData
import Lexer
import Parser

-------------------------------------------------------------------------------
-- Functions which form expression trees and operate on them.
-------------------------------------------------------------------------------

-- Creates expression tree from mathematical expression given as String.
createExpression :: String -> Tree
createExpression [] = error "No mathematical expression was given"
createExpression xs = optimize $ parse $ tokenize xs

-- Substitues variable, specified by first argument, inside expression
-- tree given as second argument, with value provided as third argument.
substitute :: String -> Tree -> Double -> Tree
substitute var (BiOpNode x t1 t2) num =
          BiOpNode x (substitute var t1 num) (substitute var t2 num)
substitute var (UnOpNode x t) num =
          UnOpNode x (substitute var t num)
substitute _ t@(NumNode _) _ = t
substitute var t@(VarNode xs) num
  | var == xs = NumNode num
  | otherwise = t

-- Evaluates expression contained inside expression tree.
evaluate :: Tree -> Double
-- Operator definitions.
evaluate (BiOpNode Plus t1 t2) = evaluate t1 + evaluate t2
evaluate (BiOpNode Minus t1 t2) = evaluate t1 - evaluate t2
evaluate (BiOpNode Mul t1 t2) = evaluate t1 * evaluate t2
evaluate (BiOpNode Div t1 t2) = evaluate t1 / evaluate t2
evaluate (BiOpNode Pow t1 t2) = evaluate t1 ** evaluate t2
-- Function definitions.
evaluate (UnOpNode Sin t) = sin $ evaluate t
evaluate (UnOpNode Cos t) = cos $ evaluate t
evaluate (UnOpNode Exp t) = exp $ evaluate t
evaluate (UnOpNode Log t) = log $ evaluate t
evaluate (UnOpNode Abs t) = abs $ evaluate t
-- Number evaluates to itself.
evaluate (NumNode num) = num
-- Variable does not evaluate and raises error.
evaluate (VarNode _) = error $
  "Tree still contains variables, " ++
  "substitute them before evaluation"

-- Derivation function exposed for use. Accepts expression tree and
-- variable for which expression tree will be derived.
deriveBy :: Tree -> String -> Tree
deriveBy t v = optimize $ deriveBy' (prepareTree t) v

-- Performs actual derivation based on the node it is currently in.
-- Derivation is propagated downstream if node is not a constant,
-- otherwise it is trivially swaped for number 0.
deriveBy' :: Tree -> String -> Tree
-- Derivation of addition propagates derivation to subtrees.
deriveBy' (BiOpNode Plus t1 t2) v = BiOpNode Plus (dinc t1 v) (dinc t2 v)
-- Derivation of subtraction propagates derivation to subtrees.
deriveBy' (BiOpNode Minus t1 t2) v = BiOpNode Minus (dinc t1 v) (dinc t2 v)
-- Derivation of multiplication:
-- (derive first) * second + first * (derive second)
deriveBy' (BiOpNode Mul t1 t2) v =
  BiOpNode Plus (BiOpNode Mul (dinc t1 v) t2) (BiOpNode Mul t1 (dinc t2 v))
-- Derivation of division:
-- ((derive first) * second - first * (derive second)) / (second ^ 2)
deriveBy' (BiOpNode Div t1 t2) v =
  BiOpNode Div
    -- Numerator
    (BiOpNode Minus (BiOpNode Mul (dinc t1 v) t2) (BiOpNode Mul t1 (dinc t2 v)))
    -- Denominator
    (BiOpNode Pow t2 (NumNode 2))
-- Derivation of power function - x ^ n: (n) * (derive x) ^ (n - 1)
deriveBy' (BiOpNode Pow t1 t2) v =
  BiOpNode Mul t2 (BiOpNode Pow (dinc t1 v) (BiOpNode Minus t2 (NumNode 1)))
-- Derivation of sin(x) = cos(x) * (derive x)
deriveBy' (UnOpNode Sin t) v = BiOpNode Mul (UnOpNode Cos t) (dinc t v)
-- Derivation of cos(x) = sin(x) * (derive x) * (-1)
deriveBy' (UnOpNode Cos t) v = BiOpNode Mul
  (UnOpNode Sin t) (BiOpNode Mul (NumNode (-1)) (dinc t v))
-- Derivation of exp(x) = exp(x) * (derive x)
deriveBy' (UnOpNode Exp t) v = BiOpNode Mul (UnOpNode Exp t) (dinc t v)
-- Derivation of log(x) = 1 / (derive x)
deriveBy' (UnOpNode Log t) v = BiOpNode Div (NumNode 1) (dinc t v)
-- Derivation of abs(x) = abs(derive x)
deriveBy' (UnOpNode Abs t) v = UnOpNode Abs $ dinc t v
-------------------------------------------------------------------------------
-- Important note for implementation.
-------------------------------------------------------------------------------
-- Variable node is here trivially returned. For this function to work properly,
-- expression tree needs to be 'prepared' before this function call. What that
-- means is that all variables which are not directly encapsulated in 'power'
-- function, will be encapsulated. This will ensure that by deriving 'power'
-- function - we effectively derived variable inside it.

-- Examples:
-- 1.) If variable is not a variable for which expression tree is derived,
-- in the earlier step 'dinc' function will discard it's subtree as constant
-- and swap it with zero.
-- 2.) If variable is a variable for which expression tree is derived,
-- in the earlier step 'power' function wrapper will be derived properly
-- and this variable can be trivially returned.

-- In this implementation, function 'prepareTree' is called as first step of
-- 'deriveBy' function and will ensure propper behaviour of this function.
-------------------------------------------------------------------------------
deriveBy' t@(VarNode _) _ = t
-- Derivation of constant is 0.
deriveBy' (NumNode _) _ = NumNode 0

-- Encapsulates variable in power function (power of 1) if it's not
-- already encapsulated in different power function. This ensures proper
-- functionality of 'deriveBy' function.
swapIfVar :: Tree -> Tree
swapIfVar v@(VarNode _) = BiOpNode Pow v (NumNode 1)
swapIfVar t             = prepareTree t

-- Prepares tree for derivation proccess by ensuring all variables are
-- properly represented in the expression tree.
prepareTree :: Tree -> Tree
prepareTree t@(BiOpNode Pow (VarNode _) _) = t
prepareTree (BiOpNode ty t1 t2) = BiOpNode ty (swapIfVar t1) (swapIfVar t2)
prepareTree (UnOpNode ty t) = UnOpNode ty (swapIfVar t)
prepareTree t@(VarNode _) = t
prepareTree t@(NumNode _) = t

-- Checks whether given (sub)tree is constant in context of derivation
-- for variable passed as second argument.
isConstant :: Tree -> String -> Bool
isConstant (BiOpNode _ t1 t2) xs = isConstant t1 xs && isConstant t2 xs
isConstant (UnOpNode _ t) xs     = isConstant t xs
isConstant (VarNode ys) xs       = xs /= ys
isConstant (NumNode _) _         = True

-- DINC - Derive If Not Constant
-- Helper method which checks whether given (sub)tree is constant,
-- and therefore can be trivially substituted with number 0 during derivation.
-- If (sub)tree is not a constant, function which executes derivation will
-- be invoked.
dinc :: Tree -> String -> Tree
dinc t xs
  | isConstant t xs = NumNode 0
  | otherwise = deriveBy' t xs

--------------------------------------------------------------------------------
-- Functions utilized for expression tree optimization.
--------------------------------------------------------------------------------

-- Performs tree optimization.
optimize :: Tree -> Tree
optimize = preEval . nubZeroes

-- Tells whether tree passed evaluates to zero.
isZero :: Tree -> Bool
isZero (BiOpNode Plus t1 t2) = isZero t1 && isZero t2
isZero (BiOpNode Minus t1 t2) = isZero t1 && isZero t2
isZero (BiOpNode Mul t1 t2) = isZero t1 || isZero t2
isZero (BiOpNode Div t1 t2)
  | isZero t2 = error "Division by zero"
  | otherwise = isZero t1
isZero (NumNode 0) = True
isZero _ = False

-- Filters tree by nubbing subtrees which evaluate to zero.
nubZeroes :: Tree -> Tree
nubZeroes t@(BiOpNode ty t1 t2)
  | isLeft && isRight = NumNode 0
  | isRight = nubZeroes t1
  | isLeft = nubZeroes t2
  | otherwise = BiOpNode ty (nubZeroes t1) (nubZeroes t2)
  where
    isLeft = isZero t1
    isRight = isZero t2
nubZeroes t@(UnOpNode ty t1)
  | isZero t = NumNode 0
  | otherwise = UnOpNode ty (nubZeroes t1)
nubZeroes t@(VarNode _) = t
nubZeroes t@(NumNode _) = t

-- Tells whether tree passed evaluates to constant.
canEvaluate :: Tree -> Bool
canEvaluate (BiOpNode _ t1 t2) = canEvaluate t1 && canEvaluate t2
canEvaluate (UnOpNode _ t)     = canEvaluate t
canEvaluate (VarNode _)        = False
canEvaluate (NumNode _)        = True

-- Filters tree by pre-evaluating subtrees which evaluate to constant.
preEval :: Tree -> Tree
preEval t@(BiOpNode ty t1 t2)
  | f1 && f2 = NumNode (evaluate t)
  | otherwise = BiOpNode ty (preEval t1) (preEval t2)
  where
    f1 = canEvaluate t1
    f2 = canEvaluate t2
preEval t@(UnOpNode ty t1)
  | canEvaluate t1 = NumNode (evaluate t)
  | otherwise = UnOpNode ty (preEval t1)
preEval t = t
