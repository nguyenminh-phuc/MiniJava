module MiniJava.Tests.Program

open System
open System.IO
open System.Text
open NUnit.Framework
open MiniJava

let isOk = function Ok _ -> true | Error _ -> false

[<TestCase("BinarySearch")>]
[<TestCase("BinaryTree")>]
[<TestCase("BubbleSort")>]
[<TestCase("Factorial")>]
[<TestCase("LinearSearch")>]
[<TestCase("LinkedList")>]
[<TestCase("QuickSort")>]
[<TestCase("TreeVisitor")>]
let ``run examples and compare console outputs`` fileName =
    let sb = StringBuilder()
    let writeLineFunc = fun x -> sb.AppendLine(string x) |> ignore
    let source = File.ReadAllText(Path.Combine("sources", fileName + ".java"))
    let input = Input.create source
    let env =
        Utils.result {
            let! tokens = Lexer.tokenize input
            let! program = Parser.parse tokens
            let! symbolTable = SymbolCollector.create program
            let! _ = TypeChecker.check symbolTable program
            let! _ = VariableInitializationChecker.check program
            let env = Environment.create symbolTable program writeLineFunc 1000L
            let! newEnv = Interpreter.interpret env
            let! assembly = ILBuilder.build symbolTable program
            let! _ = CodeGenerator.generate assembly
            return newEnv
        }
    Assert.IsTrue(isOk env)

    let result = File.ReadAllText(Path.Combine("sources", fileName + ".txt"))
    Assert.AreEqual(result.Replace("\r\n", "\n"), sb.ToString().Replace("\r\n", "\n"))

[<EntryPoint>]
let main _ = 0