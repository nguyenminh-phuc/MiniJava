using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using MiniJava.Web.Interfaces;
using MiniJava.Web.Models;

namespace MiniJava.Web.Services
{
    public class IlBuilderService : INodeBuilderService<Assembly>
    {
        public IReadOnlyCollection<Node> Build(Assembly root)
        {
            var node = new Node {Text = $"assembly: \"{root.name}\""};
            node.Add(new Node {Text = $"version: {root.version}"});
            node.Add(VisitMainModule(root.mainModule));

            return new List<Node> {node};
        }

        private static Node VisitMainModule(Module module)
        {
            var node = new Node {Text = $"mainModule: \"{module.name}\""};
            node.Add(new Node(true) {Text = "classes", Nodes = VisitClasses(module.classes)});

            return node;
        }

        private static IList<Node> VisitClasses(FSharpList<Class> classes)
        {
            var nodes = new List<Node>();
            foreach (var cls in classes)
            {
                var np = (string) typeof(Class).GetProperty("namespace'").GetValue(cls);
                var node = new Node {Text = $"class: \"{cls.name}\""};
                node.Add(new Node {Text = $"baseClassName: \"{cls.baseClassName}\""});
                node.Add(new Node {Text = $"namespace: \"{np}\""});
                node.Add(new Node(true) {Text = "attributes", Nodes = VisitAttributes(cls.attributes)});
                node.Add(new Node(true) {Text = "fields", Nodes = VisitFields(cls.fields)});
                node.Add(new Node(true) {Text = "methods", Nodes = VisitMethods(cls.methods)});
                nodes.Add(node);
            }

            return nodes;
        }

        private static IList<Node> VisitFields(FSharpList<Field> fields)
        {
            var nodes = new List<Node>();
            foreach (var field in fields)
            {
                var type = (ILType) typeof(Field).GetProperty("type'").GetValue(field);
                var node = new Node {Text = $"field: \"{field.name}\""};
                node.Add(new Node {Text = $"type: {type}"});
                node.Add(new Node(true) {Text = "attributes", Nodes = VisitAttributes(field.attributes)});
                nodes.Add(node);
            }

            return nodes.Count > 0 ? nodes : null;
        }

        private static IList<Node> VisitMethods(FSharpList<Method> methods)
        {
            var nodes = new List<Node>();
            foreach (var method in methods)
            {
                var node = new Node {Text = $"method: \"{method.name}\""};
                node.Add(new Node {Text = $"returnType: {method.returnType}"});
                node.Add(new Node(true) {Text = "attributes", Nodes = VisitAttributes(method.attributes)});
                node.Add(new Node(true) {Text = "parameters", Nodes = VisitParameters(method.parameters)});
                node.Add(new Node(true) {Text = "variables", Nodes = VisitVariables(method.variables)});
                node.Add(new Node(true) {Text = "body", Nodes = VisitBody(method.body)});
                nodes.Add(node);
            }

            return nodes.Count > 0 ? nodes : null;
        }

        private static IList<Node> VisitParameters(FSharpList<Parameter> parameters)
        {
            var nodes = new List<Node>();
            foreach (var param in parameters)
            {
                var type = (ILType) typeof(Parameter).GetProperty("type'").GetValue(param);
                var node = new Node {Text = $"parameter: \"{param.name}\""};
                node.Add(new Node {Text = $"type: {type}"});
                node.Add(new Node(true) {Text = "attributes", Nodes = VisitAttributes(param.attributes)});
                nodes.Add(node);
            }

            return nodes.Count > 0 ? nodes : null;
        }

        private static IList<Node> VisitVariables(FSharpList<Variable> variables)
        {
            var nodes = new List<Node>();
            foreach (var variable in variables)
            {
                var type = (ILType) typeof(Variable).GetProperty("type'").GetValue(variable);
                var node = new Node {Text = $"variable: {type}"};
                nodes.Add(node);
            }

            return nodes.Count > 0 ? nodes : null;
        }

        private static IList<Node> VisitBody(FSharpList<Opcode> opcodes)
        {
            var nodes = new List<Node>();
            foreach (var op in opcodes)
            {
                var node = new Node();
                switch (op)
                {
                    case Opcode.Br br:
                        node.Text = $"Br {br.Item.name}({br.Item.instruction.GetHashCode()})";
                        break;
                    case Opcode.Brtrue brtrue:
                        node.Text = $"Brtrue {brtrue.Item.name}({brtrue.Item.instruction.GetHashCode()})";
                        break;
                    case Opcode.Call call:
                        node.Text = $"Call \"{call.className}\".\"{call.methodName}\"";
                        break;
                    case Opcode.CallCtor callCtor:
                        node.Text = $"\"CallCtor\" {callCtor.className}";
                        break;
                    case Opcode.Callvirt callvirt:
                        node.Text = $"Callvirt \"{callvirt.className}\".\"{callvirt.methodName}\"";
                        break;
                    case Opcode.Label label:
                        node.Text = $"\"Label\" {label.Item.name}({label.Item.instruction.GetHashCode()})";
                        break;
                    case Opcode.Ldarg ldarg:
                        node.Text = $"Ldarg {ldarg.index}";
                        break;
                    case Opcode.Ldc_I4 ldcI4:
                        node.Text = $"Ldc_I4 {ldcI4.Item}";
                        break;
                    case Opcode.Ldfld ldfld:
                        node.Text = $"Ldfld \"{ldfld.className}\".\"{ldfld.fieldName}\"";
                        break;
                    case Opcode.Ldloc ldloc:
                        node.Text = $"Ldloc {ldloc.index}";
                        break;
                    case Opcode.Newobj newobj:
                        node.Text = $"Newobj \"{newobj.className}\"";
                        break;
                    case Opcode.Starg starg:
                        node.Text = $"Starg {starg.index}";
                        break;
                    case Opcode.Stfld stfld:
                        node.Text = $"Stfld \"{stfld.className}\".\"{stfld.fieldName}\"";
                        break;
                    case Opcode.Stloc stloc:
                        node.Text = $"Stloc {stloc.index}";
                        break;
                    default:
                        if (op.IsLdelem_I4) node.Text = "Ldelem_I4";
                        else if (op.IsLdlen)
                            node.Text = "Ldlen";
                        else if (op.IsAdd)
                            node.Text = "Add";
                        else if (op.IsSub)
                            node.Text = "Sub";
                        else if (op.IsClt)
                            node.Text = "Clt";
                        else if (op.IsMul)
                            node.Text = "Mul";
                        else if (op.IsCeq)
                            node.Text = "Ceq";
                        else if (op.IsStelem_I4)
                            node.Text = "Stelem_I4";
                        else if (op.IsWriteLine)
                            node.Text = "\"WriteLine\"";
                        else if (op.IsRet)
                            node.Text = "Ret";
                        else if (op.IsNewarr)
                            node.Text = "Newarr";
                        break;
                }

                nodes.Add(node);
            }

            return nodes;
        }

        private static IList<Node> VisitAttributes(Enum e)
        {
            return e.ToString().Split(new[] {','}).Select(flag => new Node {Text = flag}).ToList();
        }
    }
}