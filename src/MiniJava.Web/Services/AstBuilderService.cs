using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using MiniJava.Web.Interfaces;
using MiniJava.Web.Models;

namespace MiniJava.Web.Services
{
    public class AstBuilderService : INodeBuilderService<MiniJava.Program>
    {
        public IReadOnlyCollection<Node> Build(MiniJava.Program root)
        {
            var nodes = new List<Node>
            {
                VisitMainClass(root.mainClass),
                new Node(true)
                {
                    Text = "classDeclarations",
                    Nodes = VisitClassDeclarations(root.classDeclarations)
                }
            };

            return nodes;
        }

        private static Node VisitMainClass(MainClass mainClass)
        {
            var node = new Node {Text = $"mainClass: \"{mainClass.className}\""};
            node.Add(new Node {Text = $"argsName: \"{mainClass.argsName}\""});
            node.Add(VisitStatement("statement", mainClass.statement));

            return node;
        }

        private static IList<Node> VisitClassDeclarations(FSharpList<ClassDeclaration> classDeclarations)
        {
            var nodes = new List<Node>();
            foreach (var classDecl in classDeclarations)
            {
                var node = new Node {Text = $"classDeclaration: \"{classDecl.className}\""};
                node.Add(new Node {Text = $"baseClassName: \"{classDecl.baseClassName}\""});
                node.Add(new Node(true)
                {
                    Text = "fieldDeclarations",
                    Nodes = VisitVariableDeclarations(classDecl.fieldDeclarations)
                });
                node.Add(new Node(true)
                {
                    Text = "methodDeclarations",
                    Nodes = VisitMethodDeclarations(classDecl.methodDeclarations)
                });
                nodes.Add(node);
            }

            return nodes.Count > 0 ? nodes : null;
        }

        private static IList<Node> VisitVariableDeclarations(FSharpList<VariableDeclaration> variableDeclarations)
        {
            var nodes = new List<Node>();
            foreach (var varDecl in variableDeclarations)
            {
                var type = (Type) typeof(VariableDeclaration).GetProperty("type'").GetValue(varDecl);
                var node = new Node {Text = $"\"{varDecl.variableName}\": {type}"};
                nodes.Add(node);
            }

            return nodes;
        }

        private static IList<Node> VisitMethodDeclarations(FSharpList<MethodDeclaration> methodDeclarations)
        {
            var nodes = new List<Node>();
            foreach (var methodDecl in methodDeclarations)
            {
                var node = new Node {Text = $"methodDeclaration: \"{methodDecl.methodName}\""};
                node.Add(new Node {Text = $"returnType: {methodDecl.returnType}"});
                node.Add(new Node(true)
                {
                    Text = "parameterDeclarations",
                    Nodes = VisitVariableDeclarations(methodDecl.parameterDeclarations)
                });
                node.Add(new Node(true)
                {
                    Text = "variableDeclarations",
                    Nodes = VisitVariableDeclarations(methodDecl.variableDeclarations)
                });
                node.Add(new Node(true)
                {
                    Text = "body",
                    Nodes = VisitStatements(methodDecl.body)
                });
                nodes.Add(node);
            }

            return nodes.Count > 0 ? nodes : null;
        }

        private static Node VisitStatement(string label, Statement statement)
        {
            var node = new Node();
            if (label != null) node.Text = $"{label}: ";

            switch (statement)
            {
                case Statement.ArrayAssignmentStatement arrayAssignmentStatement:
                    node.Text += "arrayAssignmentStatement";
                    node.Add(new Node {Text = $"variableName: \"{arrayAssignmentStatement.variableName}\""});
                    node.Add(VisitExpression("index", arrayAssignmentStatement.index));
                    node.Add(VisitExpression("expression", arrayAssignmentStatement.expression));
                    break;
                case Statement.BlockStatement blockStatement:
                    node.ShowTags = true;
                    node.Text += "blockStatement";
                    node.Nodes = VisitStatements(blockStatement.statements);
                    break;
                case Statement.IfElseStatement ifElseStatement:
                    node.Text += "ifElseStatement";
                    node.Add(VisitExpression("condition", ifElseStatement.condition));
                    node.Add(VisitStatement("trueStatement", ifElseStatement.trueStatement));
                    node.Add(VisitStatement("falseStatement", ifElseStatement.falseStatement));
                    break;
                case Statement.PrintStatement printStatement:
                    node.Text += "printStatement";
                    node.Add(VisitExpression("expression", printStatement.expression));
                    break;
                case Statement.VariableAssignmentStatement variableAssignmentStatement:
                    node.Text += "variableAssignmentStatement";
                    node.Add(new Node {Text = $"variableName: \"{variableAssignmentStatement.variableName}\""});
                    node.Add(VisitExpression("expression", variableAssignmentStatement.expression));
                    break;
                case Statement.WhileStatement whileStatement:
                    node.Text += "whileStatement";
                    node.Add(VisitExpression("condition", whileStatement.condition));
                    node.Add(VisitStatement("statement", whileStatement.statement));
                    break;
            }

            return node;
        }

        private static IList<Node> VisitStatements(FSharpList<Statement> statements)
        {
            var nodes = new List<Node>();
            foreach (var stmt in statements)
                nodes.Add(VisitStatement(null, stmt));

            return nodes.Count > 0 ? nodes : null;
        }

        private static Node VisitExpression(string label, Expression expression)
        {
            var node = new Node();
            if (label != null) node.Text = $"{label}: ";

            switch (expression)
            {
                case Expression.ArrayInstantiationExpression arrayInstantiationExpression:
                    node.Text += "arrayInstantiationExpression";
                    node.Add(VisitExpression("size", arrayInstantiationExpression.size));
                    break;
                case Expression.ArrayLengthExpression arrayLengthExpression:
                    node.Text += "arrayLengthExpression";
                    node.Add(VisitExpression("expression", arrayLengthExpression.expression));
                    break;
                case Expression.BinaryExpression binaryExpression:
                    node.Text += "binaryExpression";
                    node.Add(VisitBinaryExpression(binaryExpression.Item));
                    break;
                case Expression.BooleanExpression booleanExpression:
                    node.Text += $"booleanExpression: {booleanExpression.value}";
                    break;
                case Expression.GroupExpression groupExpression:
                    node.Text += "groupExpression";
                    node.Add(VisitExpression("expression", groupExpression.expression));
                    break;
                case Expression.IdentifierExpression identifierExpression:
                    node.Text += $"identifierExpression: \"{identifierExpression.variableName}\"";
                    break;
                case Expression.IndexedExpression indexedExpression:
                    node.Text += "indexedExpression";
                    node.Add(VisitExpression("object", indexedExpression.@object));
                    node.Add(VisitExpression("index", indexedExpression.index));
                    break;
                case Expression.IntegerExpression integerExpression:
                    node.Text += $"integerExpression: {integerExpression.value}";
                    break;
                case Expression.MethodCallExpression methodCallExpression:
                    node.Text += "methodCallExpression";
                    node.Add(VisitExpression("object", methodCallExpression.@object));
                    node.Add(new Node {Text = $"methodName: \"{methodCallExpression.methodName}\""});
                    node.Add(new Node(true)
                    {
                        Text = "arguments",
                        Nodes = VisitExpressions(methodCallExpression.arguments)
                    });
                    break;
                case Expression.NotExpression notExpression:
                    node.Text += "notExpression";
                    node.Add(VisitExpression("exrpession", notExpression.exrpession));
                    break;
                case Expression.ObjectInstantiationExpression objectInstantiationExpression:
                    node.Text += $"objectInstantiationExpression: \"{objectInstantiationExpression.className}\"";
                    break;
            }

            return node;
        }

        private static IList<Node> VisitExpressions(FSharpList<Expression> expressions)
        {
            var nodes = new List<Node>();
            foreach (var expr in expressions)
                nodes.Add(VisitExpression(null, expr));

            return nodes.Count > 0 ? nodes : null;
        }

        private static Node VisitBinaryExpression(BinaryExpression binaryExpression)
        {
            var node = new Node();

            switch (binaryExpression)
            {
                case BinaryExpression.AdditiveExpression additiveExpression:
                    node.Text = "additiveExpression";
                    node.Add(VisitExpression("lhs", additiveExpression.lhs));
                    node.Add(VisitExpression("rhs", additiveExpression.rhs));
                    break;
                case BinaryExpression.AndExpression andExpression:
                    node.Text = "andExpression";
                    node.Add(VisitExpression("lhs", andExpression.lhs));
                    node.Add(VisitExpression("rhs", andExpression.rhs));
                    break;
                case BinaryExpression.LessThanExpression lessThanExpression:
                    node.Text = "lessThanExpression";
                    node.Add(VisitExpression("lhs", lessThanExpression.lhs));
                    node.Add(VisitExpression("rhs", lessThanExpression.rhs));
                    break;
                case BinaryExpression.MultiplicativeExpression multiplicativeExpression:
                    node.Text = "multiplicativeExpression";
                    node.Add(VisitExpression("lhs", multiplicativeExpression.lhs));
                    node.Add(VisitExpression("rhs", multiplicativeExpression.rhs));
                    break;
                case BinaryExpression.SubtractiveExpression subtractiveExpression:
                    node.Text = "subtractiveExpression";
                    node.Add(VisitExpression("lhs", subtractiveExpression.lhs));
                    node.Add(VisitExpression("rhs", subtractiveExpression.rhs));
                    break;
            }

            return node;
        }
    }
}