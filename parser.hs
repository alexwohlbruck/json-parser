import System.Environment (getArgs) -- [from source 1]
import System.IO (IOMode (ReadMode), hGetContents, hClose, openFile)
import Control.Monad (when)

import Data.List (intercalate)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt)
import Numeric (showHex)


--- Data types ---

-- Define JSON data types
data JValue =
    JNull
  | JBool Bool
  | JString String
  | JNumber Integer
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq) -- (Eq, Generic)

-- Define how to show JSON value
instance Show JValue where
  show value = case value of
    JNull           -> "null"
    JBool True      -> "true"
    JBool False     -> "false"
    JString s       -> showJSONString s
    JNumber n       -> show n
    JArray a        -> "[" ++ intercalate ", " (map show a) ++ "]"
    JObject o       -> "{" ++ intercalate ", " (map showKV o) ++ "}"
    where
        showKV (k, v) = showJSONString k ++ ": " ++ show v

-- Helper functions for Show JValue

-- Print JSON String
showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

-- Print a single character
showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/'  -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ -> [c]


-- Create a new JObject in memory for testing
-- {
--   "a": 1,
--   "b": [false, "Hello world"]
-- }
testObj = JObject[("a", JNumber 1), ("b", JArray [JBool False, JString "Hello world"])]



--- Lexical analysis ---

-- Token types
data JToken
  = LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Colon
  | StringToken String
  | NumberToken Integer
  | BoolToken Bool
  | NullToken
  deriving (Eq, Show)


preproc :: Bool -> String -> String
preproc _ "" = "" -- Base Case
preproc False ('"' : xs) = ' ' : '"' : preproc True xs -- Double quotes already inside literal mode
preproc True ('"' : xs) = '"' : ' ' : preproc False xs -- Double quotes not inside literal mode
preproc b ('[' : xs) = ' ' : '[' : ' ' : preproc b xs -- Left bracket
preproc b (']' : xs) = ' ' : ']' : ' ' : preproc b xs -- Right bracket
preproc b ('{' : xs) = ' ' : '{' : ' ' : preproc b xs -- Left brace
preproc b ('}' : xs) = ' ' : '}' : ' ' : preproc b xs -- Right brace
preproc b (',' : xs) = ' ' : ',' : ' ' : preproc b xs -- Comma
preproc b (':' : xs) = ' ' : ':' : ' ' : preproc b xs -- Colon
preproc b (x : xs) = x : preproc b xs -- All other characters

classify :: String -> JToken
classify [] = error "Token error classify: empty string."
classify "{" = LeftBrace
classify "}" = RightBrace
classify "[" = LeftBracket
classify "]" = RightBracket
classify "," = Comma
classify ":" = Colon
classify ('"' : xs) = StringToken (stringify xs)
classify ('t' : 'r' : 'u' : 'e' : xs) = BoolToken True
classify ('f' : 'a' : 'l' : 's' : 'e' : xs) = BoolToken False
classify ('n' : xs) = NullToken
classify ('-' : xs) = NumberToken (numberify xs)
classify xs = NumberToken (numberify xs) -- TODO: Only match numeric inputs, catch other characters as validation error

numberify :: String -> Integer
numberify [] = error "Token error numberify: empty string."
numberify xs = read xs

stringify :: String -> String
stringify [] = ""
stringify ('"' : xs) = stringify xs
stringify ('\\' : '"' : xs) = '"' : stringify xs
stringify ('\\' : '\\' : xs) = '\\' : stringify xs
stringify ('\\' : '/' : xs) = '/' : stringify xs
stringify ('\\' : 'b' : xs) = '\b' : stringify xs
stringify ('\\' : 'f' : xs) = '\f' : stringify xs
stringify ('\\' : 'n' : xs) = '\n' : stringify xs
stringify ('\\' : 'r' : xs) = '\r' : stringify xs
stringify ('\\' : 't' : xs) = '\t' : stringify xs
stringify (x : xs) = x : stringify xs

lexer :: String -> [JToken]
lexer s = map classify (words (preproc False s))


--- Parsing ---

-- Parse a single token
parser :: [JToken] -> JValue
parser [] = error "Parser error: empty token list."
parser (LeftBrace : xs) = JObject (parseObject xs)
parser (LeftBracket : xs) = JArray (parseArray xs)
parser (StringToken s : xs) = JString s
parser (NumberToken n : xs) = JNumber n
parser (BoolToken b : xs) = JBool b
parser (NullToken : xs) = JNull
parser (RightBrace : _) = error "Parser error: right brace without left brace."
parser (RightBracket : _) = error "Parser error: right bracket without left bracket."
parser (Comma : _) = error "Parser error: comma without left brace or left bracket."
parser (Colon : _) = error "Parser error: colon without left brace or left bracket."

-- Begin recursively parsing the json object
parseObject :: [JToken] -> [(String, JValue)]
parseObject [] = error "Parser error: empty token list."
parseObject (LeftBrace : xs) = parseObject xs
parseObject (RightBrace : _) = []
parseObject (StringToken s : Colon : xs) = (s, parser xs) : parseObject (takeWhile (/= RightBrace) xs)
parseObject (x : _) = error ("Parser error: " ++ show x ++ " is not a valid token.")

parseArray :: [JToken] -> [JValue]
parseArray [] = error "Parser error: empty token list."
parseArray (LeftBracket : xs) = parseArray xs
parseArray (RightBracket : _) = []
parseArray (x : xs) = parser (x : xs) : parseArray (dropWhile (/= RightBracket) xs)

parse :: String -> JValue
parse s = parser (lexer s)


--- User IO prompts ---

getFiles :: [String] -> (String, String)
getFiles [] = error "Error: No files provided."
getFiles [i] = error "Error: No output file provided."
getFiles [i, o] = (i, o)
getFiles (i : o : xs) = error "Error: Too many files provided."

main :: IO ()
main = do
  args <- getArgs
  let (infile, outfile) = getFiles args

  file <- openFile infile ReadMode
  text <- hGetContents file

  let lexed = lexer text
  print (show lexed)

  let parsed = show (parser lexed)
  print parsed

  writeFile outfile parsed
  hClose file