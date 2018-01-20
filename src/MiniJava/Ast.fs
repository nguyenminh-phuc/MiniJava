namespace MiniJava

type Program = Program of mainClass: MainClass * classDeclarations: ClassDeclaration list

and MainClass = MainClass of className: Identifier * argsName: Identifier * statement: Statement

and ClassDeclaration = ClassDeclaration of className: Identifier * baseClassName: Identifier option * fieldDeclarations: VariableDeclaration list * methodDeclarations: MethodDeclaration list

and VariableDeclaration = VariableDeclaration of variableName: Identifier * type': Type

and MethodDeclaration = MethodDeclaration of methodName: Identifier * returnType: Type * parameterDeclarations: VariableDeclaration list * variableDeclarations: VariableDeclaration list * body: Statement list * returnExpression: Expression

and Type =
    | ArrayType
    | IntegerType
    | BooleanType
    | ObjectType of className: Identifier

and Statement =
    | BlockStatement of statements: Statement list
    | IfElseStatement of condition: Expression * trueStatement: Statement * falseStatement: Statement
    | WhileStatement of condition: Expression * statement: Statement
    | PrintStatement of expression: Expression
    | VariableAssignmentStatement of variableName: Identifier * expression: Expression
    | ArrayAssignmentStatement of variableName: Identifier * index: Expression * expression: Expression

and Expression =
    | BinaryExpression of BinaryExpression
    | IndexedExpression of object: Expression * index: Expression
    | ArrayLengthExpression of expression: Expression
    | MethodCallExpression of object: Expression * methodName: Identifier * arguments: Expression list
    | IntegerExpression of value: int
    | BooleanExpression of value: bool
    | IdentifierExpression of variableName: Identifier
    | ThisExpression
    | ArrayInstantiationExpression of size: Expression
    | ObjectInstantiationExpression of className: Identifier
    | NotExpression of exrpession: Expression
    | GroupExpression of expression: Expression

and BinaryExpression =
    | AndExpression of lhs: Expression * rhs: Expression
    | LessThanExpression of lhs: Expression * rhs: Expression
    | AdditiveExpression of lhs: Expression * rhs: Expression
    | SubtractiveExpression of lhs: Expression * rhs: Expression
    | MultiplicativeExpression of lhs: Expression * rhs: Expression

and Identifier = string