[<RequireQualifiedAccess>]
module MiniJava.Interpreter

let checkInteger = function
    | IntegerValue value -> value
    | _ -> failwith "checkInteger: Unreachable code"

let checkBoolean = function
    | BooleanValue value -> value
    | _ -> failwith "checkBoolean: Unreachable code"

let checkArrayPointer = function
    | ArrayPointer ptrId -> ptrId
    | _ -> failwith "checkArrayPointer: Unreachable code"

let checkArray environment pointer =
    let arrayPtrId = checkArrayPointer pointer
    Environment.getHeapValue environment arrayPtrId |> function
    | ArrayValue array -> array
    | _ -> failwith "checkArray: Unreachable code"

let checkObjectPointer = function
    | ObjectPointer (ptrId, className) -> ptrId, className
    | _ -> failwith "checkObjectPointer: Unreachable code"

let checkObject environment pointer =
    let (objPtrId, _) = checkObjectPointer pointer
    Environment.getHeapValue environment objPtrId |> function
    | ObjectValue (className, fields) -> className, fields
    | _ -> failwith "checkObject: Unreachable code"

let rec visitProgram environment program =
    let (Program (mainClass, _)) = program
    let env = visitMainClass environment mainClass
    GarbageCollector.collect env

and visitMainClass environment mainClass =
    let (MainClass (_, _, stmt)) = mainClass
    visitStatement environment stmt

and visitStatement environment statement =
    let env = Environment.callGCIfRequired environment
    match statement with
    | BlockStatement stmts -> visitStatements env stmts
    | IfElseStatement (condition, trueStmt, falseStmt) ->
        let (condition, env) = visitExpression env condition
        if checkBoolean condition then visitStatement env trueStmt
        else visitStatement env falseStmt
    | WhileStatement (condition, stmt) -> visitWhileStatement env (condition, stmt)
    | PrintStatement expr ->
        let (value, env) = visitExpression env expr
        environment.writeLineFunc (checkInteger value)
        env
    | VariableAssignmentStatement (variableName, expr) ->
        let (value, env) = visitExpression env expr
        Environment.assign env variableName value
    | ArrayAssignmentStatement (variableName, index, expr) ->
        let (index, env) = visitExpression env index
        let index = checkInteger index
        let (value, env) = visitExpression env expr
        let value = checkInteger value
        Environment.assignArray env variableName index value

and visitStatements environment statements =
    statements |> List.fold (fun env stmt -> visitStatement env stmt) environment

and visitWhileStatement environment (condition, statement) =
    let (cond, env) = visitExpression environment condition
    if checkBoolean cond then
        let env = visitStatement env statement
        visitWhileStatement env (condition, statement)
    else env

and visitExpression environment = function
    | BinaryExpression expr -> visitBinaryExpression environment expr
    | IndexedExpression (obj, index) -> visitIndexedExpression environment (obj, index)
    | ArrayLengthExpression expr ->
        let (arrayPtr, env) = visitExpression environment expr
        let values = checkArray env arrayPtr
        IntegerValue values.Length, env
    | MethodCallExpression (obj, methodName, args) -> visitMethodCallExpression environment (obj, methodName, args)
    | IntegerExpression value -> (IntegerValue value, environment)
    | BooleanExpression value -> (BooleanValue value, environment)
    | IdentifierExpression variableName ->
        let value = Environment.getStackValue environment variableName
        value, environment
    | ThisExpression ->
        let thisPtr = Environment.getStackValue environment "this"
        thisPtr, environment
    | ArrayInstantiationExpression size ->
        let (size, env) = visitExpression environment size
        let (ptrId, env) = Environment.createArray env (checkInteger size)
        ArrayPointer ptrId, env
    | ObjectInstantiationExpression className ->
        let (ptrId, env) = Environment.createObject environment className
        ObjectPointer (ptrId, className), env
    | NotExpression expr ->
        let (value, env) = visitExpression environment expr
        let notValue = not (checkBoolean value)
        BooleanValue notValue, env
    | GroupExpression expr -> visitExpression environment expr

and visitBinaryExpression environment = function
    | AndExpression (lhs, rhs) ->
        let (lhsValue, env) = visitExpression environment lhs
        if checkBoolean lhsValue then visitExpression env rhs
        else BooleanValue false, env
    | LessThanExpression (lhs, rhs) ->
        let (lhsValue, env) = visitExpression environment lhs
        let (rhsValue, env) = visitExpression env rhs
        let x = checkInteger lhsValue
        let y = checkInteger rhsValue
        BooleanValue (x < y), env
    | AdditiveExpression (lhs, rhs) ->
        let (lhsValue, env) = visitExpression environment lhs
        let (rhsValue, env) = visitExpression env rhs
        let x = checkInteger lhsValue
        let y = checkInteger rhsValue
        IntegerValue (x + y), env
    | SubtractiveExpression (lhs, rhs) ->
        let (lhsValue, env) = visitExpression environment lhs
        let (rhsValue, env) = visitExpression env rhs
        let x = checkInteger lhsValue
        let y = checkInteger rhsValue
        IntegerValue (x - y), env
    | MultiplicativeExpression (lhs, rhs) ->
        let (lhsValue, env) = visitExpression environment lhs
        let (rhsValue, env) = visitExpression env rhs
        let x = checkInteger lhsValue
        let y = checkInteger rhsValue
        IntegerValue (x * y), env

and visitIndexedExpression environment (object, index) =
    let (arrayPtr, env) = visitExpression environment object
    let values = checkArray env arrayPtr
    let (index, env) = visitExpression env index
    let i = checkInteger index
    if i < 0 || i >= values.Length then failwithf "Array index is out of bounds: %d" i
    IntegerValue values.[i], env

and visitMethodCallExpression environment (object, methodName, arguments) =
    let rec visitArguments env values = function
    | arg :: args ->
         let (value, env) = visitExpression env arg
         visitArguments env (value :: values) args
    | [] -> List.rev values, env

    let (objPtr, env) = visitExpression environment object
    let (args, env) = visitArguments env [] arguments
    let (className, _) = checkObject env objPtr
    let (symbol, decl) = Environment.getMethod env className methodName true
    let (MethodSymbol (_, _, params', variables)) = symbol
    let (MethodDeclaration (_, _, _, _, body, returnExpr)) = decl
    let block = List.fold2 (fun block p arg -> Map.add p arg block) Map.empty params' args
    let block = variables |> List.fold (fun block variable -> Map.add variable NullValue block) block
    let block = block.Add (VariableSymbol ("this", ObjectType className), objPtr)
    let env = Environment.pushToStack env block
    let env = visitStatements env body
    let (returnValue, env) = visitExpression env returnExpr
    let (_, env) = Environment.popStack env
    returnValue, env

let interpret environment =
    try Ok (visitProgram environment environment.program)
    with Failure msg -> Error (sprintf "[Interpreter] %s" msg)