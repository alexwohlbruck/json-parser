import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, hClose, openFile)

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Char (isDigit, isHexDigit, isSpace, chr, ord, digitToInt, toLower)
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


delimiter = chr 0x1F -- ASCII unit separator

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

preproc :: Bool -> String -> String
preproc _ "" = "" -- Base Case
preproc True (' ' : xs) = ' ' : preproc True xs -- Space
preproc False ('"' : xs) = delimiter : '"' : preproc True xs -- Double quotes already inside literal mode
preproc True ('"' : xs) = '"' : delimiter : preproc False xs -- Double quotes not inside literal mode
preproc b ('[' : xs) = delimiter : '[' : delimiter : preproc b xs -- Left bracket
preproc b (']' : xs) = delimiter : ']' : delimiter : preproc b xs -- Right bracket
preproc b ('{' : xs) = delimiter : '{' : delimiter : preproc b xs -- Left brace
preproc b ('}' : xs) = delimiter : '}' : delimiter : preproc b xs -- Right brace
preproc b (',' : xs) = delimiter : ',' : delimiter : preproc b xs -- Comma
preproc b (':' : xs) = delimiter : ':' : delimiter : preproc b xs -- Colon
preproc b (x : xs)
  | isWhiteSpace x = preproc b xs
  | otherwise = x : preproc b xs -- All other characters


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

split :: Char -> String -> [String]
split _ [] = []
split c s = let (x, y) = break (== c) s in x : split c (drop 1 y)

lexer :: String -> [JToken]
lexer s = map classify (filter (not . null) (split delimiter (preproc False s)))


--- Parsing ---

-- takeWhile implementation that counts balanced brackets/braces
-- Keep elements of the array until the same nest-level right bracket is found
-- ex. [LeftBrace, LeftBrace, BoolToken True, RightBrace, RightBrace] -> [LeftBrace, BoolToken True, RightBrace]
--     {{true}} -> {true}
--     [LeftBrace, BoolToken True, RightBrace, LeftBrace, RightBrace] -> [BoolToken True]
--     {true}{} -> true
takeWhileBalanced :: [JToken] -> [JToken]
takeWhileBalanced [] = []
takeWhileBalanced (LeftBrace : xs) = takeWhileBrace 1 xs
takeWhileBalanced (LeftBracket : xs) = takeWhileBracket 1 xs
takeWhileBalanced xs = xs

takeWhileBrace :: Integer -> [JToken] -> [JToken]
takeWhileBrace n [] = []
takeWhileBrace n (LeftBrace : xs) = LeftBrace : takeWhileBrace (n + 1) xs
takeWhileBrace 1 (RightBrace : xs) = []
takeWhileBrace n (RightBrace : xs) = RightBrace : takeWhileBrace (n - 1) xs
takeWhileBrace n (x : xs) = x : takeWhileBrace n xs

takeWhileBracket :: Integer -> [JToken] -> [JToken]
takeWhileBracket 0 xs = []
takeWhileBracket n [] = []
takeWhileBracket n (LeftBracket : xs) = LeftBracket : takeWhileBracket (n + 1) xs
takeWhileBracket 1 (RightBracket : xs) = []
takeWhileBracket n (RightBracket : xs) = RightBracket : takeWhileBracket (n - 1) xs
takeWhileBracket n (x : xs) = x : takeWhileBracket n xs

-- Do the same for dropWhile
dropWhileBalanced :: [JToken] -> [JToken]
dropWhileBalanced [] = []
dropWhileBalanced (LeftBrace : xs) = dropWhileBrace 1 xs
dropWhileBalanced (LeftBracket : xs) = dropWhileBracket 1 xs
dropWhileBalanced xs = xs

dropWhileBrace :: Integer -> [JToken] -> [JToken]
dropWhileBrace 0 xs = xs
dropWhileBrace n [] = []
dropWhileBrace n (LeftBrace : xs) = dropWhileBrace (n + 1) xs
dropWhileBrace 1 (RightBrace : xs) = xs
dropWhileBrace n (RightBrace : xs) = dropWhileBrace (n - 1) xs
dropWhileBrace n (x : xs) = dropWhileBrace n xs

dropWhileBracket :: Integer -> [JToken] -> [JToken]
dropWhileBracket 0 xs = xs
dropWhileBracket n [] = []
dropWhileBracket n (LeftBracket : xs) = dropWhileBracket (n + 1) xs
dropWhileBracket 1 (RightBracket : xs) = xs
dropWhileBracket n (RightBracket : xs) = dropWhileBracket (n - 1) xs
dropWhileBracket n (x : xs) = dropWhileBracket n xs


-- Parse a single token
parser :: [JToken] -> JValue
parser [] = error "Parser error: empty token list."
parser (LeftBrace : xs) = JObject (parseObject (takeWhileBalanced (LeftBrace:xs)))
parser (LeftBracket : xs) = JArray (parseArray (takeWhileBalanced (LeftBracket:xs)))
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
parseObject [] = []
parseObject (Comma : xs) = parseObject xs
parseObject (LeftBrace : xs) = parseObject (takeWhileBalanced (LeftBrace:xs))
parseObject (StringToken s : Colon : LeftBrace : xs) = (s, JObject (parseObject (takeWhileBalanced (LeftBrace:xs)))) : parseObject (dropWhileBalanced (LeftBrace:xs))
parseObject (StringToken s : Colon : LeftBracket : xs) = (s, JArray (parseArray (takeWhileBalanced (LeftBracket:xs)))) : parseObject (dropWhileBalanced (LeftBracket:xs))
parseObject (StringToken s : Colon : x : xs) = (s, parser [x]) : parseObject xs
parseObject (RightBrace : _) = error "Parser error: right brace without left brace."
parseObject (x : _) = error ("Parser error: " ++ show x ++ " is not a valid token.")

parseArray :: [JToken] -> [JValue]
parseArray [] = []
parseArray (Comma : xs) = parseArray xs
parseArray (LeftBracket : xs) = parseArray (takeWhileBalanced (LeftBracket:xs))
parseArray (LeftBrace : xs) = JObject(parseObject (takeWhileBalanced (LeftBrace:xs))) : parseArray (dropWhileBalanced (LeftBrace:xs))
parseArray (RightBracket : _) = error "Parser error: right bracket without left bracket."
parseArray (x : xs) = parser [x] : parseArray xs


parse :: String -> JValue
parse s = parser (lexer s)


--- Pretty printing ---

-- Format as YAML with indentation

-- https://stackoverflow.com/questions/36746690/removing-direct-duplicates-on-a-list
compress :: String -> String
compress [] = []
compress [x] = [x]
compress (x:x2:xs) | (x == x2) && (x == '\n')   = compress (x2:xs)
                   | otherwise = x : compress (x2:xs)

yaml :: JValue -> String
yaml o = compress (yamlPrint 0 o)

yamlPrint :: Int -> JValue -> String
yamlPrint n JNull = "null"
yamlPrint n (JBool b) = map toLower (show b)
yamlPrint n (JString s) = "\"" ++ s ++ "\""
yamlPrint n (JNumber num) = show num
yamlPrint n (JObject o) = (yamlPrintObject False n o)
yamlPrint n (JArray a) = (yamlPrintArray n a)

yamlPrintObject :: Bool -> Int -> [(String, JValue)] -> String
yamlPrintObject _ _ [] = ""
yamlPrintObject False n ((s, JObject o) : xs) = (indent n) ++ s ++ ":\n" ++ (yamlPrintObject False (n + 1) o) ++ (yamlPrintObject False n xs)
yamlPrintObject False n ((s, v) : xs) = (indent n) ++ s ++ ": " ++ (yamlPrint (n+1) v) ++ "\n" ++ (yamlPrintObject False (n) xs)
yamlPrintObject True n ((s, JObject o) : xs) = " " ++ s ++ ":\n" ++ (yamlPrintObject False (n + 1) o) ++ (yamlPrintObject False n xs)
yamlPrintObject True n ((s, v) : xs) = " " ++ s ++ ": " ++ (yamlPrint (n+1) v) ++ "\n" ++ (yamlPrintObject False (n) xs)

yamlPrintArray :: Int -> [JValue] -> String
yamlPrintArray _ [] = ""
yamlPrintArray n (JObject o : xs) = "\n" ++ (indent n) ++ "-" ++ (yamlPrintObject True (n + 1) o) ++ (yamlPrintArray n xs)
yamlPrintArray n (v : xs) = "\n" ++ (indent n) ++ "- " ++ (yamlPrint (n+1) v) ++ (yamlPrintArray n xs)

-- Indent a string by n*2 spaces
indent :: Int -> String
indent n = replicate (n*2) ' '


--- User IO prompts ---

getArgs' :: [String] -> (String, String)
getArgs' [] = error "Error: No files provided."
getArgs' [i] = error "Error: No output file provided."
getArgs' [i, o] = (i, o)
getArgs' (i : o : xs) = error "Error: Too many files provided."

main :: IO ()
main = do
  args <- getArgs
  let (infile, outfile) = getArgs' args

  file <- openFile infile ReadMode
  text <- hGetContents file

  let parsed = parse text
  let output = yaml parsed

  print (output)

  writeFile outfile output
  hClose file