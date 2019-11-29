open System

let (|Upper|Lower|NotAlpha|) sym =
    if   Char.IsUpper sym then Upper
    elif Char.IsLower sym then Lower
    else NotAlpha

let encodeSymbol first last key symbolCode =
    let encodedSymbol = first + (symbolCode-first+key) % (last-first+1)
    Convert.ToChar encodedSymbol

let getCode (char: Char) =
    Convert.ToInt32 char

let getCodes c1 c2 = 
    getCode c1, getCode c2

let ( *?* )  firstLetter lastLetter = 
    getCodes firstLetter lastLetter ||> encodeSymbol

let encodeEngLowerCase = 'a' *?* 'z'
let encodeEngUpperCase = 'A' *?* 'Z'

let encodeString key str =
    let encodeUpper = encodeEngUpperCase key
    let encodeLower = encodeEngLowerCase key

    let encode sym =
        match sym with
        | Upper    -> encodeUpper <| Convert.ToInt32 sym
        | Lower    -> encodeLower <| Convert.ToInt32 sym
        | NotAlpha -> sym
        
    String.map encode str

[<EntryPoint>]
let main argv =
    encodeString  2 "Hello World from F#!" |> printfn "%s"
    encodeString -2 "Jgnnq Yqtnf htqo H#!" |> printfn "%s"
    0
