[<RequireQualifiedAccess>]
module MiniJava.Lexer

open System

let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let isDigit c = c >= '0' && c <= '9'

let isAlphaNumeric c = isAlpha c || isDigit c

let (|WhiteSpace|_|) input =
    Input.current input |> function
    | Some '\r' | Some '\n' -> Some (Input.nextLine input)
    | Some ' ' -> Some (Input.advance input)
    | _ -> None

let (|Comment|_|) input =
    let rec readOneLineComment input =
        Input.current input |> function
        | Some '\r' | Some '\n' -> Input.nextLine input
        | Some _ -> readOneLineComment (Input.advance input)
        | None -> input

    let rec readMultilineComment input =
        Input.current input |> function
        | Some '*' when Input.peek input = Some '/' -> Input.skip 2 input
        | Some _ -> readMultilineComment (Input.advance input)
        | None -> failwithf "Malformed comment at line %d" input.line

    Input.current input |> function
    | Some '/' when Input.peek input = Some '/' -> Some (readOneLineComment (Input.skip 2 input))
    | Some '/' when Input.peek input = Some '*' -> Some (readMultilineComment (Input.skip 2 input))
    | _ -> None

let (|SpecialSymbol|_|) input =
    Input.current input |> function
    | Some '(' -> Some LeftParenthesis
    | Some ')' -> Some RightParenthesis
    | Some '{' -> Some LeftCurlyBracket
    | Some '}' -> Some RightCurlyBracket
    | Some '[' -> Some LeftSquareBracket
    | Some ']' -> Some RightSquareBracket
    | Some '!' -> Some Not
    | Some '&' when Input.peek input = Some '&' -> Some And
    | Some '<' -> Some LessThan
    | Some '+' -> Some Plus
    | Some '-' -> Some Minus
    | Some '*' -> Some Multiply
    | Some '=' -> Some Assign
    | Some '.' -> Some Dot
    | Some ',' -> Some Comma
    | Some ';' -> Some Semicolon
    | _ -> None
    |> Option.bind (fun symbol -> let length = match symbol with
                                               | And -> 2
                                               | _ -> 1
                                  Some (symbol, Input.skip length input))

let (|Identifier|_|) input =
    let rec readIdentifier id input =
        Input.current input |> function
        | Some c when isAlphaNumeric c -> readIdentifier (c :: id) (Input.advance input)
        | _ -> id |> List.rev |> Array.ofList |> String, input

    Input.current input |> function
    | Some c when isAlpha c ->
        let (id, input) = readIdentifier [c] (Input.advance input)
        Token.reservedWords.TryFind id |> function
        | Some reservedWord -> Some (reservedWord, input)
        | None -> Some (TokenType.Identifier id, input)
    | _ -> None

let (|Number|_|) input =
    let rec readDigits digits input =
        Input.current input |> function
        | Some c when isDigit c -> readDigits (digits + string c) (Input.advance input)
        | _ -> digits, input

    Input.current input |> function
    | Some c when isDigit c ->
        let (integer, input') = readDigits "" input
        Int32.TryParse integer |> function
        | true, integer -> Some (IntegerLiteral integer, input')
        | _ -> failwithf "Malformed integer characters '%s' at line %d" integer input.line
    | _ -> None

let tokenize input =
    let rec tokenize tokens = function
    | WhiteSpace input | Comment input -> tokenize tokens input
    | SpecialSymbol (symbol, input) -> tokenize (symbol :: tokens) input
    | Identifier (id, input) -> tokenize (id :: tokens) input
    | Number (number, input) -> tokenize (number :: tokens) input
    | input -> Input.current input |> function
               | Some c -> failwithf "Malformed token '%c' at line %d" c input.line
               | None -> List.rev (EndOfInput :: tokens)

    try
        let tokens = tokenize [] input
        Ok tokens
    with Failure msg -> Error (sprintf "[Lexer] %s" msg)