module MiniJava.Console.Program

open System
open System.IO
open MiniJava

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: dotnet MiniJava.Console.dll MINIJAVA_SOURCE.java"
        Environment.Exit(0)

    let source = File.ReadAllText argv.[0]
    let result =
        Utils.result {
            let input = Input.create source
            let! tokens = Lexer.tokenize input
            let! program = Parser.parse tokens
            let! symbolTable = SymbolCollector.create program
            let! _ = TypeChecker.check symbolTable program
            let! _ = VariableInitializationChecker.check program
            let env = Environment.create symbolTable program (printfn "%d") 1000L
            let! newEnv = Interpreter.interpret env
            let! assembly = ILBuilder.build symbolTable program
            let! app = CodeGenerator.generate assembly
            return newEnv, assembly, app
        }

    match result with
    | Ok (env, assembly, app) ->
        printfn "Environment:\n%A" env
        printfn "Assembly:\n%A" assembly
        File.WriteAllBytes(app.assemblyName, app.assemblyContents)
        File.WriteAllText(app.configName, app.configContents)
    | Error err -> printfn "%s" err

    0