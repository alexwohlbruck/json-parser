# JSON to YAML converter

This is my final project for CS-3490 - Programming Languages, a simple JSON to YAML converter utility.

## Requirements

- Haskell
- Ghc
- A JSON file!

## Usage

1. Compile the program
```bash
ghc parser.hs
```

2. Run the program, passing the input and output file names
```bash
./parser input_file.json output_file.yaml
```

## Example

```bash
./parser example.json example.yaml
```
example.json:
```json
{
  "parent": {
    "child": 1,
    "test": true,
    "subarray": [
      "hello",
      "world",
      "Alex Wohlbruck"
    ]
  },
  "array": [
    1,
    2,
    3
  ]
}
```
example.yaml (output):
```yaml
parent:
  child: 1
  test: true
  subarray: 
    - "hello"
    - "world"
    - "Alex Wohlbruck"
array: 
  - 1
  - 2
  - 3
```

## Code structure

For those who are following the code, the code is structured as follows:

1. ### Data type definitions
  - #### `JValue`
    Data type for a JSON value. All JSON data types are supported, including:
      - `JNull`
      - `JBoolean Bool`
      - `JString String`
      - `JNumber Integer`
      - `JArray [JValue]`
      - `JObject [(String, JValue)]`
    
    This data type also uses a show instance, so any JValue can be printed as a JSON string using the show function.
  
  - #### `JToken`
    Data type for tokens produced by the lexical analysis.
      - `LeftBrace`
      - `RightBrace`
      - `LeftBracket`
      - `RightBracket`
      - `Comma`
      - `Colon`
      - `StringToken String`
      - `NumberToken Integer`
      - `BoolToken Bool`
      - `NullToken`
    
2. ### Lexical analysis
  - #### `lexer`
    The lexer function takes a string and returns a list of `JToken`s. It runs the input through `preproc` and then splits the string by the delimiter, which is defined as the ASCII control character 0x1F (unit separator). Then it calls `filter (not . null)` to remove empty strings. Finally we map the `classify` function over the list of strings the list of `JToken`s.

  - #### `preproc`
    The preproc function takes the original file input and adds delimiters between each token.

  - #### `split`
    The split function takes a string and splits it by the delimiter.

  - ### `classify`
    Here we take our individual token strings and convert them into JToken values.

  - #### `numberify`
    A helper function to convert a number into an Integer.

  - #### `stringify`
    A helper function to convert a string into a new string with quotations around it.

3. ### Parsing
  - #### `parser`
    The parser function takes a list of `JToken`s and returns a `JValue`. We have two helper functions for recusive parsing, `parseObject` and `parseArray`.
  
  - #### `parseObject`
    Helper function for parsing objects. It takes a list of `JToken`s and returns a `JObject`.

  - #### `parseArray`
    Helper function for parsing arrays. It takes a list of `JToken`s and returns a `JArray`.
  
  - #### `takeWhileBalanced` and `dropWhileBalanced`
    Helper functions for splitting a list of JTokens by the next object or array being parsed. These functions behave like the original `takeWhile` and `dropWhile` functions, but they finish when the matching closing brace/bracket is found. Both of these functions have a helper for brackets and braces.

  - #### `parse`
    This simply combines the lexer and parser functions.

4. ### Pretty printing
  - #### `yaml`
    This function takes a parsed JValue and outputs a formatted YAML string. We use compress to remove sequential newline characters (We don't want empty lines in the output).

  - #### `compress`
    This removes sequential newlines from a string.
  
  - #### `yamlPrint`
    This is the function that actaully formats the YAML string. We have two helpers, similar to the parse function for recursively processing objects and arrays.

  - #### `indent`
    This takes a number and produces a string with double that number of spaces.
  
5. ### I/O
  - #### `getFiles`
    This function takes the argument list and returns a tuple with the file names.

  - #### `main`
    This is the main function. It takes the input and output file names, calls the `parse` function and gets the pretty printed string. That string is written to the output file.

## Discussion

This was a challenging project for me, but I did learn a lot in the process. A lot of the language parsing stuff we learned in class went over my head at times and I never really nailed it until I tried to apply it with my own project. Here were some of the biggest challenges for me:

- The lexer was fairly easy until I attmped to analyze string and number literals, because you can't simply apply pattern matching rules since they can be an arbitrary length. I solved by adding a Boolean argument to the `preproc` function which I toggle when I come across a quotation mark. This prevents the preprocessor from adding delimiters in strings (In an earlier version of the program, strings with spaces would get truncated). The numberify and stringify functions handled those literals from there.

- The next thing that really challenged me was with the parser. Since JSON has a nested structure, I had to write a recursive function to parse objects and arrays. These gave me a lot of trouble but after a number of hours and lots of trial and error, I got it working. The takeWhileBalanced and dropWhileBalanced functions were the key to the problem, but I had no idea how to implement them for a while. It took me a couple days of thinking about it until I was able to sit down and write a solution.

- The last tricky part was pretty printing the YAML string from my internal data representation. I assumed this would be a pretty easy task, until I found myself yet again in recursion hell. My newline and tab chararacters were all wrong for a number of iterations until I finally found the right sequence of strings to combine. I am left with a few really ugly print functions that appear to be a submission in an esoteric programming contest, but they do work well lol.

## Notes

This parser is not a complete implementation of the JSON specification. For example, it doesn't handle comments, decimal numbers, or exponents. If I were to work on this project further, I think those are some things that I should figure out. It would also be neat if I could track the line numbers when a parsing error occurs.

Doing this project I have learned a great deal about Haskell itself and the general theory of lanugage parsing, which is something I have never tried before.

## Sources

Language parsing from scratch using Haskell:
https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/

This guy made a fully functional JSON parser that's probably way better than mine. I was planning to use his guide for most of the project but it quickly went over my head. What was useful to me was his Show instance definition for JValue. I haven't written one of those before so that was very handy to use. Although it isn't actually needed for the program to work, it was good for debugging.

Stack Overflow question on eliminating duplicate subsequent characters:
-- https://stackoverflow.com/questions/36746690/removing-direct-duplicates-on-a-list

ZVON Haskell Reference:
http://zvon.org/other/haskell/Outputglobal/index.html

This was a great resource that documents the builtin functions. I used it to help me understand how to use the `takeWhile` and `dropWhile` functions and others.