[<RequireQualifiedAccess>]
module MiniJava.Parser

let eat expectedToken = function
    | token :: tokens when token = expectedToken -> tokens
    | token :: _ -> failwithf "Token '%A' expected but token '%A' found" expectedToken token
    | [] -> failwithf "Token '%A' expected but empty token list found" expectedToken

let eatIdentifier = function
    | Identifier id :: tokens -> id, tokens
    | token :: _ -> failwithf "Identifier token expected but token '%A' found" token
    | [] -> failwith "Identifier token expected but empty token list found"

let rec program tokens =
    let (mainClass, tokens) = mainClass tokens
    let (classDeclarations, tokens) = classDeclarations tokens
    Program (mainClass, classDeclarations), tokens

and mainClass tokens =
    let tokens = eat Class tokens
    let (className, tokens) = eatIdentifier tokens
    let tokens = tokens |> eat LeftCurlyBracket
                        |> eat Public
                        |> eat Static
                        |> eat Void
                        |> eat Main
                        |> eat LeftParenthesis
                        |> eat String
                        |> eat LeftSquareBracket
                        |> eat RightSquareBracket
    let (argsName, tokens) = eatIdentifier tokens
    let tokens = tokens |> eat RightParenthesis
                        |> eat LeftCurlyBracket
    let (stmt, tokens) = statement tokens
    let tokens = tokens |> eat RightCurlyBracket
                        |> eat RightCurlyBracket
    MainClass (className, argsName, stmt), tokens

and classDeclarations tokens =
    let rec classDecls decls = function
    | Class :: tokens ->
        let (className, tokens) = eatIdentifier tokens
        let (baseClassName, tokens) =
            match tokens with
            | Extends :: tokens ->
                let (baseClassName, tokens) = eatIdentifier tokens
                Some baseClassName, tokens
            | _ -> None, tokens
        let tokens = eat LeftCurlyBracket tokens
        let (fieldDecls, tokens) = variableDeclarations tokens
        let (methodDecls, tokens) = methodDeclarations tokens
        let tokens = eat RightCurlyBracket tokens
        let decl = ClassDeclaration (className, baseClassName, fieldDecls, methodDecls)
        classDecls (decl :: decls) tokens
    | tokens -> List.rev decls, tokens

    classDecls [] tokens

and variableDeclarations tokens =
    let rec varDecls decls = function
    | Type (type', tokens) ->
        let (varName, tokens) = eatIdentifier tokens
        let tokens = eat Semicolon tokens
        let decl = VariableDeclaration (varName, type')
        varDecls (decl :: decls) tokens
    | tokens -> List.rev decls, tokens

    varDecls [] tokens

and methodDeclarations tokens =
    let rec methodDecls decls = function
    | Public :: tokens ->
        let (returnType, tokens) = type' tokens
        let (methodName, tokens) = eatIdentifier tokens
        let tokens = eat LeftParenthesis tokens
        let (paramDecls, tokens) = parameterDeclarations tokens
        let tokens = tokens |> eat RightParenthesis
                            |> eat LeftCurlyBracket
        let (varDecls, tokens) = variableDeclarations tokens
        let (body, tokens) = statements tokens
        let tokens = eat Return tokens
        let (returnExpr, tokens) = expression tokens
        let tokens = tokens |> eat Semicolon
                            |> eat RightCurlyBracket
        let decl = MethodDeclaration (methodName, returnType, paramDecls, varDecls, body, returnExpr)
        methodDecls (decl :: decls) tokens
    | tokens -> List.rev decls, tokens

    methodDecls [] tokens

and parameterDeclarations tokens =
    let rec paramDecls decls tokens =
        let (type', tokens) = type' tokens
        let (paramName, tokens) = eatIdentifier tokens
        let decl = VariableDeclaration (paramName, type')
        match tokens with
        | Comma :: tokens -> paramDecls (decl :: decls) tokens
        | _ -> match decls with
               | [] -> [decl], tokens
               | _ -> List.rev (decl :: decls), tokens

    match tokens with
    | Type _ -> paramDecls [] tokens
    | _ -> [], tokens

and type' = function
    | Int :: tokens ->
        match tokens with
        | LeftSquareBracket :: tokens ->
            let tokens = eat RightSquareBracket tokens
            ArrayType, tokens
        | _ -> IntegerType, tokens
    | Boolean :: tokens -> BooleanType, tokens
    | tokens ->
        let (id, tokens) = eatIdentifier tokens
        ObjectType id, tokens

and (|Type|_|) = function
    | Int :: tokens ->
        match tokens with
        | LeftSquareBracket :: tokens ->
            let tokens = eat RightSquareBracket tokens
            Some (ArrayType, tokens)
        | _ -> Some (IntegerType, tokens)
    | Boolean :: tokens -> Some (BooleanType, tokens)
    | Identifier _ :: Assign :: _ -> None
    | Identifier id :: tokens -> Some (ObjectType id, tokens)
    | _ -> None

and statement = function
    | BlockStatement (stmt, tokens)
    | IfElseStatement (stmt, tokens)
    | WhileStatement (stmt, tokens)
    | PrintStatement (stmt, tokens) -> (stmt, tokens)
    | tokens -> assignmentStatement tokens

and statements tokens =
    let rec statements stmts = function
    | BlockStatement (stmt, tokens)
    | IfElseStatement (stmt, tokens)
    | WhileStatement (stmt, tokens)
    | PrintStatement (stmt, tokens)
    | AssignmentStatement (stmt, tokens) -> statements (stmt :: stmts) tokens
    | tokens -> List.rev stmts, tokens

    statements [] tokens

and (|BlockStatement|_|) = function
    | LeftCurlyBracket :: tokens ->
        let (stmts, tokens) = statements tokens
        let tokens = eat RightCurlyBracket tokens
        Some (Statement.BlockStatement stmts, tokens)
    | _ -> None

and (|IfElseStatement|_|) = function
    | If :: tokens ->
        let tokens = eat LeftParenthesis tokens
        let (condition, tokens) = expression tokens
        let tokens = eat RightParenthesis tokens
        let (trueStmt, tokens) = statement tokens
        let tokens = eat Else tokens
        let (falseStmt, tokens) = statement tokens
        Some (Statement.IfElseStatement (condition, trueStmt, falseStmt), tokens)
    | _ -> None

and (|WhileStatement|_|) = function
    | While :: tokens ->
        let tokens = eat LeftParenthesis tokens
        let (condition, tokens) = expression tokens
        let tokens = eat RightParenthesis tokens
        let (stmt, tokens) = statement tokens
        Some (Statement.WhileStatement (condition, stmt), tokens)
    | _ -> None

and (|PrintStatement|_|) = function
    | System :: tokens ->
        let tokens = tokens |> eat Dot
                            |> eat Out
                            |> eat Dot
                            |> eat Println
                            |> eat LeftParenthesis
        let (expr, tokens) = expression tokens
        let tokens = tokens |> eat RightParenthesis
                            |> eat Semicolon
        Some (Statement.PrintStatement expr, tokens)
    | _ -> None

and assignmentStatement tokens =
    let (variableName, tokens) = eatIdentifier tokens
    match tokens with
    | LeftSquareBracket :: tokens ->
        let (index, tokens) = expression tokens
        let tokens = tokens |> eat RightSquareBracket
                            |> eat Assign
        let (expr, tokens) = expression tokens
        let tokens = eat Semicolon tokens
        ArrayAssignmentStatement (variableName, index, expr), tokens
    | _ ->
        let tokens = eat Assign tokens
        let (expr, tokens) = expression tokens
        let tokens = eat Semicolon tokens
        VariableAssignmentStatement (variableName, expr), tokens

and (|AssignmentStatement|_|) = function
    | Identifier _ :: _ as tokens -> Some (assignmentStatement tokens)
    | _ -> None

// http://cs.gettysburg.edu/~tneller/cs374/hw3.html
and expression tokens = andExpression tokens

and andExpression tokens =
    let rec loop lhs = function
    | And :: tokens ->
        let (rhs, tokens) = lessThanExpression tokens
        let lhs = BinaryExpression (AndExpression (lhs, rhs))
        loop lhs tokens
    | tokens -> lhs, tokens

    let (lhs, tokens) = lessThanExpression tokens
    loop lhs tokens

and lessThanExpression tokens =
    let (lhs, tokens) = additiveExpression tokens
    match tokens with
    | LessThan :: tokens ->
        let (rhs, tokens) = additiveExpression tokens
        BinaryExpression (LessThanExpression (lhs, rhs)), tokens
    | _ -> lhs, tokens

and additiveExpression tokens =
    let rec loop lhs = function
    | t :: tokens when t = Plus || t = Minus ->
        let (rhs, tokens) = multiplicativeExpression tokens
        let lhs = (match t with
                   | Plus -> AdditiveExpression (lhs, rhs)
                   | _ -> SubtractiveExpression (lhs, rhs))
                   |> BinaryExpression
        loop lhs tokens
    | tokens -> lhs, tokens
        
    let (lhs, tokens) = multiplicativeExpression tokens
    loop lhs tokens

and multiplicativeExpression tokens =
    let rec loop lhs = function
    | Multiply :: tokens ->
        let (rhs, tokens) = prefixExpression tokens
        let lhs = BinaryExpression (MultiplicativeExpression (lhs, rhs))
        loop lhs tokens
    | tokens -> lhs, tokens

    let (lhs, tokens) = prefixExpression tokens
    loop lhs tokens

and prefixExpression = function
    | Not :: tokens ->
        let (expr, tokens) = postfixExpression tokens
        NotExpression expr, tokens
    | tokens -> postfixExpression tokens

and postfixExpression tokens =
    let rec expressions exprs = function
    | RightParenthesis :: tokens -> List.rev exprs, tokens
    | tokens ->
        let (expr, tokens) = expression tokens
        match tokens with
        | Comma :: tokens -> expressions (expr :: exprs) tokens
        | _ ->
            let tokens = eat RightParenthesis tokens
            List.rev (expr :: exprs), tokens

    let (expr, tokens) = primaryExpression tokens
    match tokens with
    | LeftSquareBracket :: tokens ->
        let (index, tokens) = expression tokens
        let tokens = eat RightSquareBracket tokens
        IndexedExpression (expr, index), tokens
    | Dot :: tokens ->
        match tokens with
        | Identifier id :: tokens ->
            let tokens = eat LeftParenthesis tokens
            let (args, tokens) = expressions [] tokens
            MethodCallExpression (expr, id, args), tokens
        | _ ->
            let tokens = eat Length tokens
            ArrayLengthExpression expr, tokens
    | _ -> expr, tokens

and primaryExpression = function
    | IntegerLiteral i :: tokens -> IntegerExpression i, tokens
    | True :: tokens -> BooleanExpression true, tokens
    | False :: tokens -> BooleanExpression false, tokens
    | Identifier name :: tokens -> IdentifierExpression name, tokens
    | This :: tokens -> ThisExpression, tokens
    | LeftParenthesis :: tokens ->
        let (expr, tokens) = expression tokens
        let tokens = eat RightParenthesis tokens
        GroupExpression expr, tokens
    | tokens ->
        eat New tokens |> function
        | Int :: tokens ->
            let tokens = eat LeftSquareBracket tokens
            let (size, tokens) = expression tokens
            let tokens = eat RightSquareBracket tokens
            ArrayInstantiationExpression size, tokens
        | tokens ->
            let (id, tokens) = eatIdentifier tokens
            let tokens = tokens |> eat LeftParenthesis
                                |> eat RightParenthesis
            ObjectInstantiationExpression id, tokens

let parse tokens =
    try
        let (program, tokens) = program tokens
        match tokens with
        | [EndOfInput] -> ()
        | token :: _ -> failwithf "Token '%A' expected but token '%A' found" EndOfInput token
        | _ -> failwithf "Token '%A' expected at the end of token list" EndOfInput
        Ok program
    with Failure msg -> Error (sprintf "[Parser] %s" msg)