namespace MiniJava

type ResultBuilder() =
    member __.Bind (x, f) = match x with
                            | Ok value -> f value
                            | Error error -> Error error
    member __.Return x = Ok x

type MaybeBuilder() =
    member __.Bind (x, f) = match x with
                            | Some a -> f a
                            | None -> None
    member __.Return x = Some x
    member __.ReturnFrom x = x

type OrElseBuilder() =
    member __.Combine (a, b) = match a with
                               | Some _ -> a
                               | None -> b
    member __.Delay f = f()
    member __.ReturnFrom x = x

[<RequireQualifiedAccess>]
module Utils =

    let result = ResultBuilder()

    let maybe = MaybeBuilder()

    let orElse = OrElseBuilder()

    let toString x = sprintf "%A" x