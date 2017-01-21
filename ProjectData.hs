module ProjectData where

-- Value constructors for input tokenization.
-- TokOper stores operators.
-- TokFunc stores identifiers which are specified as functions.
-- TokIden stores identifiers. Identifiers start with letter and
-- and can further contain letters and numbers.
-- TokNumb stores numbers.
-- TokLPar stores left parentheses.
-- TokRPar stores right parentheses.
data Token = TokOper Operator
           | TokFunc Function
           | TokIden String
           | TokNumb Double
           | TokLPar
           | TokRPar
    deriving (Show, Eq)

-- Value constructors for operator representation.
data Operator = Plus | Minus | Mul | Div | Pow
    deriving (Eq)

-- Manually defining Operator as instance of Show type class for custom print.
instance Show Operator where
  show Plus  = "+"
  show Minus = "-"
  show Mul   = "*"
  show Div   = "/"
  show Pow   = "^"

-- Supported operators.
supportedOperators :: String
supportedOperators = "+-*/^"

-- Retrieves coresponding operator for given character.
-- If character does not represent supported operator, raises error.
operator :: Char -> Operator
operator c
  | c == '+' = Plus
  | c == '-' = Minus
  | c == '*' = Mul
  | c == '/' = Div
  | c == '^' = Pow
  | otherwise = error "Unsupported operator"

-- Retrieves precedence for given operator in form of integer value.
precedence :: Operator -> Int
precedence Plus  = 2
precedence Minus = 2
precedence Mul   = 3
precedence Div   = 3
precedence Pow   = 4

-- Retrieves left associativity for given operator as boolean value.
-- Operators which are not left associative are right associative.
leftAssociativity :: Operator -> Bool
leftAssociativity Pow = False
leftAssociativity _   = True

-- Value constructors for function representation.
data Function = Sin | Cos | Exp | Log | Abs
    deriving (Eq)

-- Manually defining Function as instance of Show type class for custom print.
instance Show Function where
  show Sin = "sin"
  show Cos = "cos"
  show Exp = "exp"
  show Log = "log"
  show Abs = "abs"

-- Supported functions.
supportedFunctions :: [String]
supportedFunctions = ["sin","cos","exp","log","abs"]

-- Retrieves coresponding function for given string.
-- If string does not represent supported function, error is raised.
function :: String -> Function
function s
  | s == "sin" = Sin
  | s == "cos" = Cos
  | s == "exp" = Exp
  | s == "log" = Log
  | s == "abs" = Abs
  | otherwise = error "Unsupported function"

-- Tree data structure for expression representation.
-- BiOpNode are binary operations and have two children.
-- UnOpNode are unary operations and have one child.
-- VarNode stores variable constant.
-- NumNode stores number constant.
data Tree = BiOpNode Operator Tree Tree
          | UnOpNode Function Tree
          | VarNode String
          | NumNode Double

instance Show Tree where

  -- BINARY OPERATIONS.
  show (BiOpNode ty t1@(VarNode _) t2@(VarNode _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(NumNode _) t2@(NumNode _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(UnOpNode _ _) t2@(UnOpNode _ _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2

  show (BiOpNode ty t1@(VarNode _) t2@(NumNode _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(NumNode _) t2@(VarNode _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(UnOpNode _ _) t2@(NumNode _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(NumNode _) t2@(UnOpNode _ _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(UnOpNode _ _) t2@(VarNode _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2
  show (BiOpNode ty t1@(VarNode _) t2@(UnOpNode _ _)) =
    show t1 ++ " " ++ show ty ++ " " ++ show t2

  -- Single variable constant.
  show (BiOpNode ty t1@(VarNode _) t2) =
    show t1 ++ " " ++ show ty ++ " ( " ++ show t2 ++ " ) "
  show (BiOpNode ty t1 t2@(VarNode _)) =
    " ( " ++ show t1 ++ " ) " ++ show ty ++ " " ++ show t2
  -- Single number constant.
  show (BiOpNode ty t1@(NumNode _) t2) =
    show t1 ++ " " ++ show ty ++ " ( " ++ show t2 ++ " ) "
  show (BiOpNode ty t1 t2@(NumNode _)) =
    " ( " ++ show t1 ++ " ) " ++ show ty ++ " " ++ show t2
  -- Single function.
  show (BiOpNode ty t1@(UnOpNode _ _) t2) =
    show t1 ++ " " ++ show ty ++ " ( " ++ show t2 ++ " ) "
  show (BiOpNode ty t1 t2@(UnOpNode _ _)) =
    " ( " ++ show t1 ++ " ) " ++ show ty ++ " " ++ show t2
  -- No variable and no number and no function.
  show (BiOpNode ty t1 t2) =
    " ( " ++ show t1 ++ " ) " ++ show ty ++ " ( " ++ show t2 ++ " ) "

  -- UNARY OPERATIONS.
  show (UnOpNode ty t) = show ty ++ " ( " ++ show t ++ " ) "

  -- Variable.
  show (VarNode var)          = var
  -- Number.
  show (NumNode num)          = show num
