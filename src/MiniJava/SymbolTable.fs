namespace MiniJava

type SymbolTable = SymbolTable of classes: ClassSymbol list

and ClassSymbol = ClassSymbol of className: Identifier * baseClassName: Identifier option * fields: VariableSymbol list * methods: MethodSymbol list

and MethodSymbol = MethodSymbol of methodName: Identifier * returnType: Type * parameters: VariableSymbol list * variables: VariableSymbol list

and VariableSymbol = VariableSymbol of variableName: Identifier * type': Type

[<RequireQualifiedAccess>]
module SymbolTable =

    let getMainClass (SymbolTable classes) = classes.Head

    let findClass (SymbolTable classes) className =
        classes |> List.tryFind (fun (ClassSymbol (name, _, _, _)) -> name = className)

    let rec findMethod symbolTable className methodName findBaseClasses =
        Utils.maybe {
            let! (ClassSymbol (_, baseClassName, _, methods)) = findClass symbolTable className
            let method = methods |> List.tryFind (fun (MethodSymbol (name, _, _, _)) -> name = methodName)
            match method, baseClassName with
            | Some method, _ -> return method
            | None, Some baseClassName when findBaseClasses ->
                return! findMethod symbolTable baseClassName methodName true
            | None, _ -> return! None
        }

    let rec findField symbolTable className fieldName findBaseClasses =
        Utils.maybe {
            let! (ClassSymbol (_, baseClassName, fields, _)) = findClass symbolTable className
            let field = fields |> List.tryFind (fun (VariableSymbol (name, _)) -> name = fieldName)
            match field, baseClassName with
            | Some field, _ -> return field
            | None, Some baseClassName when findBaseClasses ->
                return! findField symbolTable baseClassName fieldName true
            | None, _ -> return! None
        }

    let findLocalVariable symbolTable className methodName variableName =
        Utils.maybe {
            let! (MethodSymbol (_, _, params', variables)) = findMethod symbolTable className methodName false
            return! Utils.orElse {
                return! variables |> List.tryFind (fun (VariableSymbol (name, _)) -> name = variableName)
                return! params' |> List.tryFind (fun (VariableSymbol (name, _)) -> name = variableName)
            }
        }

    let findVariable symbolTable className methodName variableName findBaseClasses =
        Utils.orElse {
            return! findLocalVariable symbolTable className methodName variableName
            return! findField symbolTable className variableName findBaseClasses
        }