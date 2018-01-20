[<RequireQualifiedAccess>]
module MiniJava.TypeChecker

let checkClass symbolTable className =
    SymbolTable.findClass symbolTable className |> function
    | Some class' -> class'
    | None -> failwithf "Class '%s' not found" className

let checkMethod symbolTable className methodName findBaseClasses =
    SymbolTable.findMethod symbolTable className methodName findBaseClasses |> function
    | Some method -> method
    | None -> failwithf "Method '%s' in class '%s' not found" methodName className

let checkVariable symbolTable className methodName variableName findBaseClasses =
    match methodName with
    | Some methodName ->
        SymbolTable.findVariable symbolTable className methodName variableName findBaseClasses |> function
        | Some variable -> variable
        | None -> failwithf "Variable '%s' in method '%s' in class '%s' not found" variableName methodName className
    | None -> failwithf "Main class '%s' cannot declare variables" className

let checkType symbolTable = function
    | ObjectType className -> checkClass symbolTable className |> ignore
    | _ -> ()

let rec checkCyclicInheritance symbolTable className = function
    | Some baseClassName ->
        if className = baseClassName then failwithf "Cyclic inheritance involving '%s'" className
        let (ClassSymbol (_, baseClassName, _, _)) = checkClass symbolTable baseClassName
        checkCyclicInheritance symbolTable className baseClassName
    | None -> ()

let rec expectObjectType symbolTable baseClassName className =
    if baseClassName = className then ()
    else let (ClassSymbol (_, baseClassName', _, _)) = checkClass symbolTable className
         match baseClassName' with
         | Some baseClassName' -> expectObjectType symbolTable baseClassName baseClassName'
         | None -> failwithf "Incompatible types: '%s' cannot be converted to '%s'" baseClassName className

let expectType symbolTable expectedType type' =
    match expectedType, type' with
    | ObjectType baseClassName, ObjectType className -> expectObjectType symbolTable baseClassName className
    | _ when expectedType = type' -> ()
    | _ -> failwithf "Incompatible types: '%A' cannot be converted to '%A'" type' expectedType

let checkOverrideMethod symbolTable (className, methodName) =
    let rec visitArguments params1 params2 =
        match params1, params2 with
        | VariableSymbol (_, type1) :: params1, VariableSymbol (_, type2) :: param2 ->
            expectType symbolTable type1 type2
            visitArguments params1 param2
        | _ -> ()

    let rec check baseReturnType baseParams = function
    | Some baseClassName ->
        let (ClassSymbol (className', baseClassName, _, methods)) = checkClass symbolTable baseClassName
        methods |> List.tryFind (fun (MethodSymbol (name, _, _, _)) -> name = methodName) |> function
        | Some (MethodSymbol (_, returnType, params', _)) ->
            try
                expectType symbolTable baseReturnType returnType
                visitArguments baseParams params'
            with Failure msg ->
                failwithf "Method '%s' in class '%s' cannot override method in base class '%s': %s" methodName className' className msg
        | None -> check baseReturnType baseParams baseClassName
    | None -> ()

    let (ClassSymbol (_, baseClassName, _, _)) = checkClass symbolTable className
    let (MethodSymbol (_, baseReturnType, baseParams, _)) = checkMethod symbolTable className methodName false
    check baseReturnType baseParams baseClassName

let rec visitProgram symbolTable program =
    let (Program (mainClass, classDecls)) = program
    mainClass |> visitMainClass symbolTable
    classDecls |> List.iter (fun classDecl -> classDecl |> visitClassDeclaration symbolTable)

and visitMainClass symbolTable mainClass =
    let (MainClass (className, _, stmt)) = mainClass
    checkClass symbolTable className |> ignore
    stmt |> visitStatement symbolTable (className, None)

and visitClassDeclaration symbolTable classDeclaration =
    let (ClassDeclaration (className, baseClassName, fieldDecls, methodDecls)) = classDeclaration
    checkClass symbolTable className |> ignore
    checkCyclicInheritance symbolTable className baseClassName
    fieldDecls  |> visitVariableDeclarations symbolTable
    methodDecls |> visitMethodDeclarations symbolTable className

and visitVariableDeclarations symbolTable variableDeclarations =
    variableDeclarations |> List.iter (fun (VariableDeclaration (_, type')) -> checkType symbolTable type')

and visitMethodDeclaration symbolTable className methodDeclaration =
    let (MethodDeclaration (methodName, returnType, paramDecls, varDecls, body, returnExpr)) = methodDeclaration
    checkMethod symbolTable className methodName false |> ignore
    checkType symbolTable returnType
    paramDecls |> visitVariableDeclarations symbolTable
    varDecls   |> visitVariableDeclarations symbolTable
    body       |> visitStatements symbolTable (className, Some methodName)
    returnExpr |> visitExpression symbolTable (className, Some methodName)
               |> expectType symbolTable returnType
    checkOverrideMethod symbolTable (className, methodName)

and visitMethodDeclarations symbolTable className methodDeclarations =
    methodDeclarations |> List.iter (fun methodDecl -> methodDecl |> visitMethodDeclaration symbolTable className)

and visitStatement symbolTable (className, methodName) = function
    | BlockStatement stmts -> stmts |> visitStatements symbolTable (className, methodName)
    | IfElseStatement (condition, trueStmt, falseStmt) ->
        condition |> visitExpression symbolTable (className, methodName)
                  |> expectType symbolTable BooleanType
        trueStmt  |> visitStatement symbolTable (className, methodName)
        falseStmt |> visitStatement symbolTable (className, methodName)
    | WhileStatement (condition, stmt) ->
        condition |> visitExpression symbolTable (className, methodName)
                  |> expectType symbolTable BooleanType
        stmt      |> visitStatement symbolTable (className, methodName)
    | PrintStatement expr ->
        expr |> visitExpression symbolTable (className, methodName)
             |> expectType symbolTable IntegerType
    | VariableAssignmentStatement (variableName, expr) ->
        let (VariableSymbol (_, type')) = checkVariable symbolTable className methodName variableName true
        expr |> visitExpression symbolTable (className, methodName)
             |> expectType symbolTable type'
    | ArrayAssignmentStatement (variableName, index, expr) ->
        let (VariableSymbol (_, type')) = checkVariable symbolTable className methodName variableName true
        expectType symbolTable ArrayType type'
        expr  |> visitExpression symbolTable (className, methodName)
              |> expectType symbolTable IntegerType
        index |> visitExpression symbolTable (className, methodName)
              |> expectType symbolTable IntegerType

and visitStatements symbolTable (className, methodName) statements =
    statements |> List.iter (fun stmt -> stmt |> visitStatement symbolTable (className, methodName))

and visitExpression symbolTable (className, methodName) = function
    | BinaryExpression expr -> expr |> visitBinaryExpression symbolTable (className, methodName)
    | IndexedExpression (obj, index) ->
        obj   |> visitExpression symbolTable (className, methodName)
              |> expectType symbolTable ArrayType
        index |> visitExpression symbolTable (className, methodName)
              |> expectType symbolTable IntegerType
        IntegerType
    | ArrayLengthExpression expr ->
        expr |> visitExpression symbolTable (className, methodName)
             |> expectType symbolTable ArrayType
        IntegerType
    | MethodCallExpression (obj, callingMethodName, args) ->
        (obj, callingMethodName, args) |> visitMethodCallExpression symbolTable (className, methodName)
    | IntegerExpression _ -> IntegerType
    | BooleanExpression _ -> BooleanType
    | IdentifierExpression variableName ->
        let (VariableSymbol (_, type')) = checkVariable symbolTable className methodName variableName true
        type'
    | ThisExpression ->
        let (ClassSymbol (mainClassName, _, _, _)) = SymbolTable.getMainClass symbolTable
        if className = mainClassName then failwith "Non-static variable 'this' cannot be referenced from a static context"
        ObjectType className
    | ArrayInstantiationExpression size ->
        size |> visitExpression symbolTable (className, methodName)
             |> expectType symbolTable IntegerType
        ArrayType
    | ObjectInstantiationExpression className ->
        checkClass symbolTable className |> ignore
        ObjectType className
    | NotExpression expr ->
        expr |> visitExpression symbolTable (className, methodName)
             |> expectType symbolTable BooleanType
        BooleanType
    | GroupExpression expr -> expr |> visitExpression symbolTable (className, methodName)

and visitBinaryExpression symbolTable (className, methodName) = function
    | AndExpression (lhs, rhs) ->
        lhs |> visitExpression symbolTable (className, methodName)
            |> expectType symbolTable BooleanType
        rhs |> visitExpression symbolTable (className, methodName)
            |> expectType symbolTable BooleanType
        BooleanType
    | LessThanExpression (lhs, rhs) ->
        lhs |> visitExpression symbolTable (className, methodName)
            |> expectType symbolTable IntegerType
        rhs |> visitExpression symbolTable (className, methodName)
            |> expectType symbolTable IntegerType
        BooleanType
    | AdditiveExpression (lhs, rhs)
    | SubtractiveExpression (lhs, rhs)
    | MultiplicativeExpression (lhs, rhs) ->
        lhs |> visitExpression symbolTable (className, methodName)
            |> expectType symbolTable IntegerType
        rhs |> visitExpression symbolTable (className, methodName)
            |> expectType symbolTable IntegerType
        IntegerType

and visitMethodCallExpression symbolTable (className, methodName) (object, callingMethodName, arguments) =
    let rec visitArguments callingClassName params' args =
        match params', args with
        | VariableSymbol (_, type') :: params', arg :: args ->
            arg |> visitExpression symbolTable (className, methodName)
                |> expectType symbolTable type'
            visitArguments callingClassName params' args
        | [], [] -> ()
        | _ -> failwithf "Cannot call method '%s' of class '%s': actual and formal argument lists differ in length" callingMethodName callingClassName

    object |> visitExpression symbolTable (className, methodName) |> function
    | ObjectType callingClassName ->
        checkClass symbolTable callingClassName |> ignore
        let (MethodSymbol (_, returnType, params', _)) = checkMethod symbolTable callingClassName callingMethodName true
        visitArguments callingClassName params' arguments
        returnType
    | t -> failwithf "Cannot call method '%s' of non-complex type '%A'" callingMethodName t

let check symbolTable program =
    try Ok (visitProgram symbolTable program)
    with Failure msg -> Error (sprintf "[TypeChecker] %s" msg)