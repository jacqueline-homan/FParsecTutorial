open System
open System.IO
open FParsec

type UserState = unit 
type Parser<'t> = Parser<'t, UserState>

// Parsing a single float
let test (p:Parser<_,_>) str =
    match run p str with 
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25" //This should show Success
test pfloat "1.25E 3" //This should give an error message
test pfloat "42" //Nota Bena: Integers are treated as floats


(*This lets us know of a word exists in a string collection. 
if it does, Success is returned, otherwise Failure showing 
us exactly what failed (in our case, "input" fails since 
the Parser is expecting a 'Result of "test"). This also parses
as single float
*) 
let str s = pstring s
let floatBetweenBrackets(s:string) = str "[" >>. pfloat .>> str "]"
let p : Parser<_> = pstring "1.25"
let q : Parser<_> = pstring "test"
test p "input"
test p "1.25"
test q "test"

(* This will return an array of string elements showing the elements
immediately preceeding and following then ".", which are 1 and 2
respectively:
*)
let x : Parser<_> = pstring "Object: 1.23"
let run x (s:string) : string [] = s.Split('.')
run x "1.23" |> printfn "%A"

//Parsing a float between brackets
let floatBetweenBrackets2 = str "[" >>. pfloat .>> str "]"
test floatBetweenBrackets2 "[2.0]"
test floatBetweenBrackets2 "[]"
test floatBetweenBrackets2 "[2.0"

//Abstracting parsers
let betweenStrings s1 s2 px = str s1 >>. px .>> str s2 
let floatBetweenBrackets3 = pfloat |> betweenStrings "[" "]"
let floatBetweenDblBrackets = pfloat |> betweenStrings "[[" "]]"

test floatBetweenBrackets3 "[1.0]"
test floatBetweenBrackets3 "[]"
test floatBetweenBrackets3 "[1.0"
test floatBetweenDblBrackets "[[1.0]]"

(*Need to examine this further and experiment more
let between pbegin pend p = pbegin >>. p .>> pend
let betwStrgs s1 s2 p = p |> between (str s1) (str s2)
betwStrgs "[abc; def]" "[poopy; cakes]" |> printfn "%A"
let run2 p (s:string) : string [] = s.Split(';')
run2 p "[a; bc; d; ef]" |> printfn "%A" 
*)

// Parsing a list of floats
test (many floatBetweenBrackets3) ""
test (many floatBetweenBrackets3) "[1.0]"
test (many floatBetweenBrackets3) "[2][3][4]"
test (many floatBetweenBrackets3) "[1][2.0E]"

test (many1 floatBetweenBrackets3) "(1)"

test (many1 (floatBetweenBrackets3 <?> "float between brackets")) "(1)"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]"
test floatList "[1.0]"
test floatList "[4,5,6]"
test floatList "[1.0,"
test floatList "[your momma]" //Expect a fail: "your momma" isn't a number

// Handling whitespaces
let ws = spaces

let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList @"[ 1 ,
                          2 ] "

test numberList @"[ 1,
                         2; 3]"

let numberListFile = ws >>. numberList .>> eof
test numberListFile " [1, 2, 3] [4]"

//Parsing strings
test (many (str "a" <|> str "b")) "abba"

//To parse a case-insensitive string, use pstring CI and skipStringCI
test (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0" //returns Success: 1.0

//To parse an identifier
let identifier =
    //returns Boolean value of True if c is a letter or underscore
    let isIdentifierFirstChar c = isLetter c || c = '_'
    //returns Boolean value of True if c is a letter, digit, or underscore
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    //calls combinator and two functions that return a Boolean, and skips trailing whitespace
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
test identifier "_" //returns Success
test identifier "_test1" //returns Success
test identifier "1" //returns Failure - "Expecting: identifier"
printfn "\n"

(*To parse identifiers based on the Unicode XID syntax:
stringLiteral: '"' (normalChar|escapedChar)* '"'
normalChar:    any char except '\' and '"'
escapedChar:   '\\' ('\\'|'"'|'n'|'r'|'t')
*)
let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\""
test stringLiteral "\"abc\\\"def\\\\ghi\""
test stringLiteral "\"abc\\def\""

(* Instead of parsing the string literal char-by-char, we can also
parse it "snippet-by-snippet." Here, we used then manyStrings combinator
which parses as sequence of stringsSepBy with then given string parser 
and returns then stringsSepBy in concatenated form:
*)
let stringLiteral2 =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (manyStrings (normalCharSnippet <|> escapedChar))

test stringLiteral2 "\"abc\""
test stringLiteral2 "\"abc\\\"def\\\\ghi\""
test stringLiteral2 "\"abc\\def\""
printfn "\n"

(*Parsing a string chunk‐wise using an optimized parser like many1Satisfy is 
usually faster than parsing it char‐wise using manyChars and satisfy. 
In this case we can optimize our parser even further – once we realize 
that two normal char snippets must be separated by at least one escaped char.

The stringsSepBy combinator parses a sequence of strings 
(with the first argument parser) separated by other strings 
(parsed with the second argument parser). It returns all parsed strings, including
the separator strings, as a single, concatenated string.

Note that stringLiteral3 uses manySatisfy instead of many1Satisfy in its 
normalCharSnippet definition, so that it can parse escaped chars that
are not separated by normal chars. 

This can’t lead to an infinite loop because escapedChar 
can’t succeed without consuming input.
*)
let stringLiteral3 =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

test stringLiteral3 "\"abc\""
test stringLiteral3 "\"abc\\\"def\\\\ghi\""
test stringLiteral3 "\"abc\\def\""
printfn "\n"

(*Sequentially applying parsers and the use of the pipe2
through pipe5 combinators.

val pipe2: Parser<'a,'u> -> Parser<'b,'u> -> ('a -> b -> 'c) -> Parser<'c,'u> 
*)
printfn "Using pipe2 to parse the product of two numbers:"
let product = pipe2 float_ws (str_ws "*" >>. float_ws)
                    (fun x y -> x * y)

test product "3 * 5"

printfn "\n"
(*The pipe2-5 combinators are very useful for constructing AST objects. 
In the following example we use pipe3 to parse a string constant definition 
into a StringConstant object: *)
printfn "Using pipe3 to parse a string constant definition into a StringConstant object:"

type StringConstant = StringConstant of string * string

let stringConstant = pipe3 identifier (str_ws "=") stringLiteral
                           (fun id _ str -> StringConstant(id, str))

test stringConstant "myString = \"stringValue\""

printfn "\n"

let tupled1 = pipe2 float_ws (str_ws "," >>. float_ws)
                    (fun x y -> (x, y))
test tupled1 "3 , 5"

test (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456" 

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

