namespace MiniJava

type VariableScope =
    | LocalScope of index: int * type': Type
    | ParameterScope of index: int * type': Type
    | FieldScope of className: Identifier * type': Type

[<RequireQualifiedAccess>]
module ILBuilder =

    open System
    open Mono.Cecil
    open Mono.Cecil.Cil

    let rec findField symbolTable className fieldName findBaseClasses =
        Utils.maybe {
            let! (ClassSymbol (className, baseClassName, fields, _)) = SymbolTable.findClass symbolTable className
            let field = fields |> List.tryFind (fun (VariableSymbol (name, _)) -> name = fieldName)
            match field, baseClassName with
            | Some (VariableSymbol (_, type')), _ -> return FieldScope (className, type')
            | None, Some baseClassName when findBaseClasses ->
                return! findField symbolTable baseClassName fieldName true
            | None, _ -> return! None
        }

    let findLocalVariable symbolTable className methodName variableName =
        Utils.maybe {
            let! (MethodSymbol (_, _, params', variables)) = SymbolTable.findMethod symbolTable className methodName false
            return! variables |> List.tryFindIndex (fun (VariableSymbol (name, _)) -> name = variableName) |> function
            | Some index ->
                let (VariableSymbol (_, type')) = variables.[index]
                Some (LocalScope (index, type'))
            | None ->
                params' |> List.tryFindIndex (fun (VariableSymbol (name, _)) -> name = variableName) |> function
                | Some index ->
                    let (VariableSymbol (_, type')) = params'.[index]
                    Some (ParameterScope ((index + 1), type'))
                | None -> None
        }

    let findVariable symbolTable className methodName variableName findBaseClasses =
        let methodName = Option.get methodName
        Utils.orElse {
            return! findLocalVariable symbolTable className methodName variableName
            return! findField symbolTable className variableName findBaseClasses
        }

    let getOptionClass = function
    | Type.ObjectType className -> Some className
    | _ -> None

    let getILType = function
    | ArrayType -> IntegerArrayType
    | Type.IntegerType -> IntegerType
    | Type.BooleanType -> BooleanType
    | Type.ObjectType className -> ObjectType className

    let createLabel name =
        { name = name
          instruction = Instruction.Create(OpCodes.Nop) }

    let rec visitProgram symbolTable program =
        let (Program (MainClass (className, _, _) as mainClass, classDecls)) = program
        let mainClass = mainClass |> visitMainClass symbolTable
        let classes = classDecls |> List.map (fun classDecl -> visitClassDeclaration symbolTable className classDecl)
        let module' =
            { name = className + ".dll"
              classes = mainClass :: classes }
        { name = className
          version = Version(1, 0, 0, 0)
          mainModule = module' }

    and visitMainClass symbolTable mainClass =
        let (MainClass (className, argsName, stmt)) = mainClass
        let ctor = visitConstructorDeclaration None
        let mainMethod =
            { name = "Main"
              returnType = VoidType
              attributes = MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig
              parameters = [{ name = argsName; type' = StringArrayType; attributes = ParameterAttributes.None }]
              variables = []
              body = visitStatement symbolTable (className, None) stmt @ [Ret] }
        { namespace' = className
          name = className
          baseClassName = None
          attributes = TypeAttributes.BeforeFieldInit
          fields = []
          methods = ctor :: [mainMethod] }

    and visitClassDeclaration symbolTable namespace' classDeclaration =
        let (ClassDeclaration (className, baseClassName, fieldDecls, methodDecls)) = classDeclaration
        let ctor = visitConstructorDeclaration baseClassName
        { namespace' = namespace'
          name = className
          baseClassName = baseClassName
          attributes = TypeAttributes.BeforeFieldInit
          fields = visitFieldDeclarations fieldDecls
          methods = ctor :: visitMethodDeclarations symbolTable className methodDecls }

    and visitFieldDeclarations fieldDeclarations =
        let rec visitFieldDecls fields = function
        | VariableDeclaration (fieldName, type') :: fieldDecls ->
            let field =
                { Field.name = fieldName
                  type' = getILType type'
                  attributes = FieldAttributes.Assembly }
            visitFieldDecls (field :: fields) fieldDecls
        | [] -> List.rev fields

        visitFieldDecls [] fieldDeclarations

    and visitConstructorDeclaration baseClassName =
        { name = ".ctor"
          returnType = VoidType
          attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
          parameters = []
          variables = []
          body = [Ldarg 0; CallCtor baseClassName; Ret] }

    and visitMethodDeclarations symbolTable className methodDeclarations =
        let rec visitMethodDecls methods = function
        | MethodDeclaration (methodName, returnType, paramDecls, variableDecls, body, returnExpr) :: methodDecls ->
            let opcodes =
                [ visitStatements symbolTable (className, Some methodName) body
                  visitExpression symbolTable (className, Some methodName) returnExpr |> fst
                  [Ret] ] |> List.concat
            let method =
                { name = methodName
                  returnType = getILType returnType
                  attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig
                  parameters = visitParameterDeclarations paramDecls
                  variables = variableDecls |> List.map (fun (VariableDeclaration (_, type')) -> { type' = getILType type' })
                  body = opcodes }
            visitMethodDecls (method :: methods) methodDecls
        | [] -> List.rev methods

        visitMethodDecls [] methodDeclarations

    and visitParameterDeclarations parameterDeclarations =
        let rec visitParamDecls params' = function
        | VariableDeclaration (paramName, type') :: paramDecls ->
            let param =
                { name = paramName
                  type' = getILType type'
                  attributes = ParameterAttributes.None }
            visitParamDecls (param :: params') paramDecls
        | [] -> List.rev params'

        visitParamDecls [] parameterDeclarations

    and visitStatement symbolTable (className, methodName) = function
        | BlockStatement stmts ->
            stmts |> List.collect (fun stmt -> visitStatement symbolTable (className, methodName) stmt)
        | IfElseStatement (condition, ifStmt, elseStmt) ->
            let thenLabel = createLabel "thenLabel"
            let endLabel = createLabel "endLabel"
            [ visitExpression symbolTable (className, methodName) condition |> fst
              [Brtrue thenLabel]
              visitStatement symbolTable (className, methodName) elseStmt
              [Br endLabel]
              [Label thenLabel]
              visitStatement symbolTable (className, methodName) ifStmt
              [Label endLabel] ] |> List.concat
        | WhileStatement (condition, stmt) ->
            let startLabel = createLabel "startLabel"
            let conditionLabel = createLabel "conditionLabel"
            [ [Br conditionLabel]
              [Label startLabel]
              visitStatement symbolTable (className, methodName) stmt
              [Label conditionLabel]
              visitExpression symbolTable (className, methodName) condition |> fst
              [Brtrue startLabel] ] |> List.concat
        | PrintStatement expr ->
            [ visitExpression symbolTable (className, methodName) expr |> fst
              [WriteLine] ] |> List.concat
        | VariableAssignmentStatement (variableName, expr) ->
            let exprOpcodes = visitExpression symbolTable (className, methodName) expr |> fst
            findVariable symbolTable className methodName variableName true |> Option.get |> function
            | LocalScope (index, _) -> exprOpcodes @ [Stloc index]
            | ParameterScope (index, _) -> exprOpcodes @ [Starg index]
            | FieldScope (className, _) -> [Ldarg 0] @ exprOpcodes @ [Stfld (variableName, className)]
        | ArrayAssignmentStatement (variableName, index, expr) ->
            [ visitIdentifierExpression symbolTable (className, methodName) variableName |> fst
              visitExpression symbolTable (className, methodName) index |> fst 
              visitExpression symbolTable (className, methodName) expr |> fst
              [Stelem_I4] ] |> List.concat

    and visitStatements symbolTable (className, methodName) statements =
        let rec visitStmts opcodes = function
        | stmt :: stmts ->
            let stmtOpcodes = visitStatement symbolTable (className, methodName) stmt
            visitStmts (opcodes @ stmtOpcodes) stmts
        | [] -> opcodes

        visitStmts [] statements

    and VariableAssignmentStatement symbolTable (className, methodName) (variableName, expression) =
        let exprOpcodes = visitExpression symbolTable (className, methodName) expression |> fst
        findVariable symbolTable className methodName variableName true |> Option.get |> function
        | LocalScope (index, _) -> exprOpcodes @ [Stloc index]
        | ParameterScope (index, _) -> exprOpcodes @ [Starg index]
        | FieldScope (className, _) -> [Ldarg 0] @ exprOpcodes @ [Stfld (variableName, className)]

    and visitExpression symbolTable (className, methodName) = function
        | BinaryExpression expr -> visitBinaryExpression symbolTable (className, methodName) expr
        | IndexedExpression (obj, index) ->
            [ visitExpression symbolTable (className, methodName) obj |> fst
              visitExpression symbolTable (className, methodName) index |> fst
              [Ldelem_I4] ] |> List.concat, None
        | ArrayLengthExpression expr ->
            [ visitExpression symbolTable (className, methodName) expr |> fst
              [Ldlen] ] |> List.concat, None
        | MethodCallExpression (obj, callingMethodName, args) ->
            visitMethodCallExpression symbolTable (className, methodName) (obj, callingMethodName, args)
        | IntegerExpression value -> [Ldc_I4 value], None
        | BooleanExpression value -> (if value then [Ldc_I4 1] else [Ldc_I4 0]), None
        | IdentifierExpression variableName ->
            visitIdentifierExpression symbolTable (className, methodName) variableName
        | ThisExpression -> [Ldarg 0], Some className
        | ArrayInstantiationExpression size ->
            [ visitExpression symbolTable (className, methodName) size |> fst
              [Newarr] ] |> List.concat, None
        | ObjectInstantiationExpression className ->
            [Newobj className], Some className
        | NotExpression expr ->
            [ visitExpression symbolTable (className, methodName) expr |> fst
              [Ldc_I4 0]
              [Ceq] ] |> List.concat, None
        | GroupExpression expr -> visitExpression symbolTable (className, methodName) expr

    and visitBinaryExpression symbolTable (className, methodName) = function
        | AndExpression (lhs, rhs) ->
            let lhsIsTrueLabel = createLabel "lhsIsTrueLabel"
            let endLabel = createLabel "endLabel"
            [ visitExpression symbolTable (className, methodName) lhs |> fst
              [Brtrue lhsIsTrueLabel]
              [Ldc_I4 0]
              [Br endLabel]
              [Label lhsIsTrueLabel]
              visitExpression symbolTable (className, methodName) rhs |> fst
              [Label endLabel] ] |> List.concat, None
        | LessThanExpression (lhs, rhs) ->
            [ visitExpression symbolTable (className, methodName) lhs |> fst
              visitExpression symbolTable (className, methodName) rhs |> fst
              [Clt] ] |> List.concat, None
        | AdditiveExpression (lhs, rhs) ->
            [ visitExpression symbolTable (className, methodName) lhs |> fst
              visitExpression symbolTable (className, methodName) rhs |> fst
              [Add] ] |> List.concat, None
        | SubtractiveExpression (lhs, rhs) ->
            [ visitExpression symbolTable (className, methodName) lhs |> fst
              visitExpression symbolTable (className, methodName) rhs |> fst
              [Sub] ] |> List.concat, None
        | MultiplicativeExpression (lhs, rhs) ->
            [ visitExpression symbolTable (className, methodName) lhs |> fst
              visitExpression symbolTable (className, methodName) rhs |> fst
              [Mul] ] |> List.concat, None

    and visitMethodCallExpression symbolTable (className, methodName) (object, callingMethodName, arguments) =
        let (objOpcodes, type') = visitExpression symbolTable (className, methodName) object
        let callingClassName = Option.get type'
        let (MethodSymbol (_, returnType, _, _)) =
            SymbolTable.findMethod symbolTable callingClassName callingMethodName true
            |> Option.get
        [ objOpcodes
          arguments |> List.collect (fun arg -> visitExpression symbolTable (className, methodName) arg |> fst)
          (if callingClassName = className then [Call (callingMethodName, className)]
          else [Callvirt (callingMethodName, callingClassName)]) ] |> List.concat, getOptionClass returnType

    and visitIdentifierExpression symbolTable (className, methodName) variableName =
        findVariable symbolTable className methodName variableName true |> Option.get |> function
        | LocalScope (index, type') -> [Ldloc index], getOptionClass type'
        | ParameterScope (index, type') -> [Ldarg index], getOptionClass type'
        | FieldScope (className, type') -> [Ldarg 0; Ldfld (variableName, className)], getOptionClass type'

    let build symbolTable program =
        try Ok (visitProgram symbolTable program)
        with Failure msg -> Error (sprintf "[ILBuilder] %s" msg)