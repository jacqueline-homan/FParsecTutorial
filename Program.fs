open System
open System.IO
open FParsec

type UserState = unit 
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with 
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25" //This should show Success
test pfloat "1.25E 3" //This should give an error message
test pfloat "42" //Nota Bene: Integers are treated as floats


(*This lets us know of a word exists in a string collection. 
if it does, Success is returned, otherwise Failure showing 
us exactly what failed (in our case, "input" fails since 
the Parser is expecting a 'Result of "test")
*) 
let str s = pstring s
let floatBetweenBrackets(s:string) = str "[" >>. pfloat .>> str "]"
let p : Parser<_> = pstring "test"
test p "input"
test p "test"

(* This will return an array of string elements showing the elements
immediately preceeding and following then "." 
*)
let x : Parser<_> = pstring "Object: 1.23"
let run x (s:string) : string [] = s.Split('.')
run x "1.23" |> printfn "%A"


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

