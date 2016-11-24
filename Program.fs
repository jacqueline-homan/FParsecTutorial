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

(*To parse identifiers based on the Unicode XID syntax:
*)
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

