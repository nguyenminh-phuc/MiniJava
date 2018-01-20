[<RequireQualifiedAccess>]
module MiniJava.VariableInitializationChecker

let declareVariables initializedVariables variableDeclarations =
    variableDeclarations
    |> List.fold (fun initVars (VariableDeclaration(variableName, _)) -> Set.add variableName initVars) initializedVariables

let checkInitializedVariable initializedVariables variableName =
    if not (Set.contains variableName initializedVariables) then
        failwithf "Variable '%s' might not have been initialized" variableName

let rec visitProgram program =
    let (Program (_, classDecls)) = program
    classDecls |> List.iter visitClassDeclaration

and visitClassDeclaration classDeclaration =
    let (ClassDeclaration (_, _, fieldDecls, methodDecls)) = classDeclaration
    let initVars = declareVariables Set.empty fieldDecls
    methodDecls |> List.iter (fun methodDecl -> visitMethodDeclaration initVars methodDecl)

and visitMethodDeclaration initializedVariables methodDeclaration =
    let (MethodDeclaration (_, _, paramDecls, _, body, returnExpr)) = methodDeclaration
    let initVars = declareVariables initializedVariables paramDecls
    let initVars = visitStatements initVars body
    visitExpression initVars returnExpr |> ignore

and visitStatement initializedVariables = function
    | BlockStatement stmts -> visitStatements initializedVariables stmts
    | IfElseStatement (condition, trueStmt, falseStmt) ->
        visitIfElseStatement initializedVariables (condition, trueStmt, falseStmt)
    | WhileStatement (condition, stmt) ->
        let condition = visitExpression initializedVariables condition
        let whileInitVars = visitStatement initializedVariables stmt
        match condition with
        | Some (BooleanExpression true) -> whileInitVars
        | _ -> initializedVariables
    | PrintStatement expr ->
        visitExpression initializedVariables expr |> ignore
        initializedVariables
    | VariableAssignmentStatement (variableName, expr) ->
        visitExpression initializedVariables expr |> ignore
        initializedVariables.Add(variableName)
    | ArrayAssignmentStatement (variableName, index, expr) ->
        checkInitializedVariable initializedVariables variableName
        visitExpression initializedVariables index |> ignore
        visitExpression initializedVariables expr |> ignore
        initializedVariables

and visitIfElseStatement initializedVariables (condition, trueStatement, falseStatement) =
    let condition = visitExpression initializedVariables condition
    let trueInitVars = visitStatement initializedVariables trueStatement
    let falseInitVars = visitStatement initializedVariables falseStatement
    match condition with
    | Some (BooleanExpression true) -> trueInitVars
    | Some (BooleanExpression false) -> falseInitVars
    | _ -> Set.intersect trueInitVars falseInitVars

and visitStatements initializedVariables statements =
    statements |> List.fold (fun initVars stmt -> visitStatement initVars stmt) initializedVariables

and visitExpression initializedVariables = function
    | BinaryExpression expr -> visitBinaryExpression initializedVariables expr
    | IndexedExpression (obj, index) ->
        visitExpression initializedVariables obj |> ignore
        visitExpression initializedVariables index |> ignore
        None
    | MethodCallExpression (obj, methodName, args) ->
        visitMethodCallExpression initializedVariables (obj, methodName, args)
        None
    | IntegerExpression value -> Some (IntegerExpression value)
    | BooleanExpression value -> Some (BooleanExpression value)
    | IdentifierExpression variableName ->
        checkInitializedVariable initializedVariables variableName
        None
    | ArrayLengthExpression expr
    | ArrayInstantiationExpression expr ->
        visitExpression initializedVariables expr |> ignore
        None
    | ObjectInstantiationExpression _
    | ThisExpression -> None
    | NotExpression expr
    | GroupExpression expr -> visitExpression initializedVariables expr

and visitBinaryExpression initializedVariables = function
    | AndExpression (lhs, rhs) ->
        let lhs = visitExpression initializedVariables lhs
        let rhs = visitExpression initializedVariables rhs
        match lhs, rhs with
        | Some (BooleanExpression lhs), Some (BooleanExpression rhs) -> Some (BooleanExpression (lhs && rhs))
        | _ -> None
    | LessThanExpression (lhs, rhs) ->
        let lhs = visitExpression initializedVariables lhs
        let rhs = visitExpression initializedVariables rhs
        match lhs, rhs with
        | Some (IntegerExpression lhs), Some (IntegerExpression rhs) -> Some (BooleanExpression (lhs < rhs))
        | _ -> None
    | AdditiveExpression (lhs, rhs) ->
        let lhs = visitExpression initializedVariables lhs
        let rhs = visitExpression initializedVariables rhs
        match lhs, rhs with
        | Some (IntegerExpression lhs), Some (IntegerExpression rhs) -> Some (IntegerExpression (lhs + rhs))
        | _ -> None
    | SubtractiveExpression (lhs, rhs) ->
        let lhs = visitExpression initializedVariables lhs
        let rhs = visitExpression initializedVariables rhs
        match lhs, rhs with
        | Some (IntegerExpression lhs), Some (IntegerExpression rhs) -> Some (IntegerExpression (lhs - rhs))
        | _ -> None
    | MultiplicativeExpression (lhs, rhs) ->
        let lhs = visitExpression initializedVariables lhs
        let rhs = visitExpression initializedVariables rhs
        match lhs, rhs with
        | Some (IntegerExpression lhs), Some (IntegerExpression rhs) -> Some (IntegerExpression (lhs * rhs))
        | _ -> None

and visitMethodCallExpression initializedVariables (object, _, arguments) =
    visitExpression initializedVariables object |> ignore
    arguments |> List.iter (fun arg -> visitExpression initializedVariables arg |> ignore)

let check program =
    try Ok (visitProgram program)
    with Failure msg -> Error (sprintf "[InitializationChecker] %s" msg)