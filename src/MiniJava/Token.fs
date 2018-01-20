namespace MiniJava

type TokenType =
    | LeftParenthesis
    | RightParenthesis
    | LeftCurlyBracket
    | RightCurlyBracket
    | LeftSquareBracket
    | RightSquareBracket
    | Not
    | And
    | LessThan
    | Plus
    | Minus
    | Multiply
    | Assign
    | Dot
    | Comma
    | Semicolon
    | Class
    | Public
    | Static
    | Void
    | Main
    | String
    | Extends
    | Int
    | Boolean
    | If
    | Else
    | While
    | System
    | Out
    | Println
    | Return
    | This
    | Length
    | New
    | True
    | False
    | Identifier of string
    | IntegerLiteral of int
    | EndOfInput

[<RequireQualifiedAccess>]
module Token =

    let reservedWords =
        [ "class", Class
          "public", Public
          "static", Static
          "void", Void
          "main", Main
          "String", String
          "extends", Extends
          "int", Int
          "boolean", Boolean
          "if", If
          "else", Else
          "while", While
          "System", System
          "out", Out
          "println", Println
          "return", Return
          "this", This
          "length", Length
          "new", New
          "true", True
          "false", False ] |> Map.ofList