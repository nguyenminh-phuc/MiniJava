[<RequireQualifiedAccess>]
module MiniJava.SymbolCollector

let rec visitProgram program =
    let (Program (mainClass, classDecls)) = program
    let mainClass = visitMainClass mainClass
    visitClassDeclarations [mainClass] classDecls

and visitMainClass mainClass =
    let (MainClass (className, _, _)) = mainClass
    ClassSymbol (className, None, [], [])

and visitClassDeclaration classDeclaration =
    let (ClassDeclaration (className, baseClassName, fieldDecls, methodDecls)) = classDeclaration
    let variables = visitVariableDeclarations className None fieldDecls
    let methods = visitMethodDeclarations className methodDecls
    ClassSymbol (className, baseClassName, variables, methods)

and visitClassDeclarations classes = function
    | classDecl :: classDecls ->
        let (ClassSymbol (className, _, _, _)) as class' = visitClassDeclaration classDecl
        if classes |> List.exists (fun (ClassSymbol (name, _, _, _)) -> name = className) then
            failwithf "Duplicate class '%s'" className
        visitClassDeclarations (class' :: classes) classDecls
    | [] -> List.rev classes

and visitVariableDeclarations className methodName variableDeclarations =
    let rec visitVarDecls variables = function
    | VariableDeclaration (variableName, type') :: varDecls ->
        if variables |> List.exists (fun (VariableSymbol (name, _)) -> name = variableName) then
            match methodName with
            | Some methodName -> failwithf "Variable '%s' is already defined in method '%s' in class '%s'" variableName methodName className
            | None -> failwithf "Field '%s' is already defined in class '%s'" variableName className
        let variable = VariableSymbol (variableName, type')
        visitVarDecls (variable :: variables) varDecls
    | [] -> List.rev variables

    visitVarDecls [] variableDeclarations

and visitMethodDeclaration className methodDeclaration =
    let (MethodDeclaration (methodName, returnType, paramDecls, varDecls, _, _)) = methodDeclaration
    let params' = visitVariableDeclarations className (Some methodName) paramDecls
    let variables = visitVariableDeclarations className (Some methodName) varDecls
    params' |> List.iter (fun (VariableSymbol (paramName, _) as param) ->
                             if List.contains param variables then
                                 failwithf "Variable '%s' is already defined in method '%s' in class '%s'" paramName methodName className)
    MethodSymbol (methodName, returnType, params', variables)

and visitMethodDeclarations className methodDecls =
    let rec visitMethodDecls methods = function
    | methodDecl :: methodDecls ->
        let (MethodSymbol (methodName, _, _, _)) as method = visitMethodDeclaration className methodDecl
        if methods |> List.exists (fun (MethodSymbol (name, _, _, _)) -> name = methodName) then
            failwithf "Cannot overload method '%s' in class '%s'" methodName className
        visitMethodDecls (method :: methods) methodDecls
    | [] -> List.rev methods

    visitMethodDecls [] methodDecls

let create program =
    try
        let symbolTable = SymbolTable (visitProgram program)
        Ok symbolTable
    with Failure msg -> Error (sprintf "[SymbolCollector] %s" msg)