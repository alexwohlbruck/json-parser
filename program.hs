

-- Type declarations and helper functions

-- Variables
type Vars = String
-- Arithmetic expressions
data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
  deriving Show
-- Boolean expressions
data BExpr = TT | FF          -- the true and false constants
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
           | Eql AExpr AExpr  -- equality of arithmetic expressions
           | Lt  AExpr AExpr  -- true if the first is less than the second
           | Lte AExpr AExpr  -- true if it's less than or equal to
  deriving Show
-- Instructions
data Instr = Assign Vars AExpr            -- assign X to the value of an expression
           | IfThenElse BExpr Instr Instr -- conditional
           | While BExpr Instr            -- looping construct
           | Do [Instr]                   -- a block of several instructions
           | Nop                          -- the "do nothing" instruction
  deriving Show

-- A program is a list of instructions
type Program = [Instr]

-- Environments
type Env = [(Vars,Integer)]

update :: (Vars, Integer) -> Env -> Env
update (x,newval) [] = [(x,newval)]
update (x,newval) ((y,v) : e) | x == y = (x,newval) : e
                            | otherwise = (y,v) : update (x,newval) e

lookUp :: Vars -> Env -> Integer
lookUp x e = case (lookup x e) of (Just v) -> v


-- Question 1.  Evaluators for arithmetic and boolean expressions

evala :: Env -> AExpr -> Integer
evala env (Var x) = lookUp x env
evala env (Const v) = v
evala env (Add p1 p2) = evala env p1 + evala env p2
evala env (Sub p1 p2) = evala env p1 - evala env p2
evala env (Mul p1 p2) = evala env p1 * evala env p2
evala env (Div p1 p2) = evala env p1 `div` evala env p2

evalb :: Env -> BExpr -> Bool
evalb _ TT = True
evalb _ FF = False
evalb e (And b1 b2) = evalb e b1 && evalb e b2
evalb e (Or  b1 b2) = evalb e b1 || evalb e b2
evalb e (Not b) = not $ evalb e b
evalb e (Eql e1 e2) = evala e e1 == evala e e2
evalb e (Lt e1 e2)  = evala e e1 < evala e e2
evalb e (Lte e1 e2) = evala e e1 <= evala e e2


-- Question 2.  Executing instructions.

exec :: Instr -> Env -> Env
exec (Assign x v) e = update (x, evala e v) e
exec (IfThenElse c i1 i2) e = if evalb e c then exec i1 e else exec i2 e
exec (While c i) e = if evalb e c then exec (While c i) (exec i e) else e
exec (Do []) e = e
exec (Do (i:is)) e = exec (Do is) (exec i e)
exec Nop e = e

run :: Program -> Env
run p = exec (Do p) []

-- Example from assignment
sum100 :: Program     -- a program to add together all the numbers up to 100
sum100 = [
  Assign "X" (Const 0),         -- initialize the sum     at X=0
  Assign "C" (Const 1),         -- initialize the counter at C=1
  While (Lt (Var "C") (Const 101))    -- while C < 101, do:
        (Do [Assign "X" (Add (Var "X") (Var "C")),  -- X := X + C;
             Assign "C" (Add (Var "C") (Const 1))]  -- C := C + 1
        )]

sum100output = lookUp "X" (run sum100)


-- Question 3.  Lexical analysis

data UOps = NotOp deriving Show

data BOps = AddOp | SubOp | MulOp | DivOp
          | AndOp | OrOp  | EqlOp | LtOp  | LteOp  | AssignOp
  deriving Show

data Token = VSym String | CSym Integer | BSym Bool
           | UOp UOps | BOp BOps
           | LPar | RPar | LBra | RBra | Semi
           | Keyword String
           | Err String
           | PA AExpr | PB BExpr | PI Instr
           | Block [Instr]
  deriving Show

-- helper functions (can instead be imported from Data.Char)
isAlpha :: Char -> Bool
isAlpha x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) =
  let q1 "" = True
      q1 (y:ys) = if isDigit y || isAlpha y || y == '-' || y == '_'
                    then q1 ys
                    else if y == '\'' then q2 ys else False
      q2 "" = True
      q2 ('\'':ys) = q2 ys
      q2 _ = False
  in  isAlpha x && q1 xs

isCSym :: String -> Bool
isCSym = all isDigit

-- classify is not strictly necessary;
-- it identifies a string as a token of the correct type
classify :: String -> Token

classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify "*" = BOp MulOp
classify "/" = BOp DivOp

classify "!" = UOp NotOp
classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp
classify "==" = BOp EqlOp
classify "<" = BOp LtOp
classify "<=" = BOp LteOp
classify ":=" = BOp AssignOp

classify "(" = LPar
classify ")" = RPar
classify "{" = LBra
classify "}" = RBra
classify ";" = Semi

classify "while" = Keyword "while"
classify "if" = Keyword "if"
classify "then" = Keyword "then"
classify "else" = Keyword "else"
classify "do" = Keyword "do"
classify "nop" = Keyword "nop"

classify "T" = BSym True
classify "F" = BSym False
classify x | isCSym x = CSym (read x)
classify x | isVSym x = VSym x
classify x = Err x

-- this approach to lexical analysis makes use of a preprocessor
addSpaces :: String -> String
addSpaces "" = ""
addSpaces ('/' : '\\' : s) = " /\\ " ++ addSpaces s
addSpaces ('\\' : '/' : s) = " \\/ " ++ addSpaces s
addSpaces ('<' : '=' : s)  = " <= " ++ addSpaces s
addSpaces (':' : '=' : s)  = " := " ++ addSpaces s
addSpaces ('=' : '=' : s)  = " == " ++ addSpaces s
addSpaces ('!' : s) = " ! " ++ addSpaces s
addSpaces ('*' : s) = " * " ++ addSpaces s
addSpaces ('/' : s) = " / " ++ addSpaces s
addSpaces ('+' : s) = " + " ++ addSpaces s
addSpaces ('-' : s) = " - " ++ addSpaces s
addSpaces ('<' : s) = " < " ++ addSpaces s
addSpaces ('(' : s) = " ( " ++ addSpaces s
addSpaces (')' : s) = " ) " ++ addSpaces s
addSpaces (';' : s) = " ; " ++ addSpaces s
addSpaces (x : s) = x : addSpaces s

-- the actual lexer is now trivial
lexer :: String -> [Token]
lexer s = map classify (words (addSpaces s))


-- Question 4. Parsing

parser :: [Token] -> Instr
parser l = sr (LBra : l ++ [RBra]) []

-- The main parsing function alternates between shifting elements
-- from the input queue to the parse stack, and reducing the top of the stack
sr :: [Token] -> [Token] -> Instr
-- variables and constants
sr i (VSym x : ts)     = sr i (PA (Var  x) : ts)
sr i (CSym x : ts)     = sr i (PA (Const x) : ts)
sr i (BSym True : ts)  = sr i (PB TT : ts)
sr i (BSym False : ts) = sr i (PB FF : ts)
-- boolean operations
sr i (PB p : UOp NotOp : ts)          = sr i (PB (Not p) : ts)
sr i (PB p2 : BOp AndOp : PB p1 : ts) = sr i (PB (And p1 p2) : ts)
sr i (PB p2 : BOp OrOp  : PB p1 : ts) = sr i (PB (Or  p1 p2) : ts)
-- arithmetic operations
sr i (PA p2 : BOp AddOp : PA p1 : ts) = sr i (PA (Add p1 p2) : ts)
sr i (PA p2 : BOp SubOp : PA p1 : ts) = sr i (PA (Sub p1 p2) : ts)
sr i (PA p2 : BOp MulOp : PA p1 : ts) = sr i (PA (Mul p1 p2) : ts)
sr i (PA p2 : BOp DivOp : PA p1 : ts) = sr i (PA (Div p1 p2) : ts)
-- comparisons and assignment operation
sr i (PA p2 : BOp EqlOp : PA p1 : ts) = sr i (PB (Eql p1 p2) : ts)
sr i (PA p2 : BOp LtOp  : PA p1 : ts) = sr i (PB (Lt p1 p2) : ts)
sr i (PA p2 : BOp LteOp : PA p1 : ts) = sr i (PB (Lte p1 p2) : ts)
sr i (PA p : BOp AssignOp : PA (Var x) : ts) = sr i (PI (Assign x p) : ts)
-- keywords
sr i (PI i0 : PB b : Keyword "while" : ts) = sr i (PI (While b i0) : ts)
sr i (Keyword "nop" : ts)                 = sr i (PI Nop : ts)
sr i (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : ts)
    = sr i (PI (IfThenElse b i1 i2) : ts)
-- parentheses
sr i (RPar : PA p : LPar : ts)        = sr i (PA p : ts)
sr i (RPar : PB p : LPar : ts)        = sr i (PB p : ts)
sr i (RBra : PI p : LBra : ts)        = sr i (PI p : ts)
-- parsing do blocks
sr inp (RBra : PI i : ts)            = sr inp (Block [i] : ts)
sr inp (RBra : ts)                   = sr inp (Block []  : ts)
-- sr inp (RBra : Semi : PI i : ts)     = sr inp (Block [i] : ts)
sr inp (Block is : Semi : PI i : ts) = sr inp (Block (i:is) : ts)
-- sr inp (Block is : PI i : ts)        = sr inp (Block (i:is) : ts)
sr inp (Block is : LBra : ts)        = sr inp (PI (Do is) : ts)
-- shift
sr (i:is) xs = sr is (i:xs)
-- termination
sr [] [PI p] = p
-- error s
sr _ (Err s : _) = error $ "Lexical error: " ++ take 10 s
sr [] x = error $ "Parser error: " ++ show x


-- Question 5. I/O

main :: IO ()
main = do
  putStrLn "Enter a file to load"
  filename <- getLine
  input <- readFile filename
  let lexed = lexer input
  let parsed = parser lexed
  let result = run [parsed]
  putStrLn (show result)


-- Examples/Test Cases

fibonacci8 :: String
fibonacci8 = -- you can break strings over many lines by connecting them with \
  "x:=0; \
  \ y:=1; \
  \ c:=0; \
  \ while (c<7) \\/ (c==7) do ( \
  \  c:= (c+1); \
  \  z:= (x+y); \
  \  x:=y; \
  \  y:=z ) "

fibLexed :: [Token]
fibLexed = lexer fibonacci8

fibParsed :: Instr
fibParsed = parser fibLexed

fibResult :: Integer
fibResult = lookUp "x" (run [fibParsed])

factorial5 :: String
factorial5 =
    " fact:=1; \
    \ c :=1 ;  \
    \ while (! (4 < c)) do ( \
    \   c := (c+1); \
    \   fact := (fact*c )\
    \ )"

factLexed = lexer factorial5
factParsed = parser ((LPar : factLexed) ++ [RPar])
factResult = lookUp "fact" (run [factParsed])