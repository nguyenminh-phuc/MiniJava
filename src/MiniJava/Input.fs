namespace MiniJava

type Input =
    { source: string
      index: int
      line: int }

[<RequireQualifiedAccess>]
module Input =

    let create source =
        { source = source
          index = 0
          line = 1 }

    let current input =
        if input.index >= input.source.Length then None
        else Some input.source.[input.index]

    let advance input = { input with index = input.index + 1 }

    let peek input =
        if input.index + 1 >= input.source.Length then None
        else Some input.source.[input.index + 1]

    let skip n input = { input with index = input.index + n }

    let nextLine input =
        let i = if current input = Some '\r' && peek input = Some '\n' then 2
                else 1
        { input with index = input.index + i
                     line = input.line + 1 }