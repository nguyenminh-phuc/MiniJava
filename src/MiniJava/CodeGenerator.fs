namespace MiniJava

type Application =
    { assemblyName: string
      assemblyContents: byte[]
      configName: string
      configContents: string }

[<RequireQualifiedAccess>]
module CodeGenerator =

    open System
    open System.Linq
    open System.IO
    open Mono.Cecil
    open Mono.Cecil.Cil

    let runtimeConfigJson = """
    {
      "runtimeOptions": {
        "tfm": "netcoreapp2.0",
        "framework": {
          "name": "Microsoft.NETCore.App",
          "version": "2.0.0"
        }
      }
    }
    """

    let rec visitAssembly assembly =
        let module' = assembly.mainModule
        let ilAssemblyName = AssemblyNameDefinition(assembly.name, assembly.version)
        let ilAssembly = AssemblyDefinition.CreateAssembly(ilAssemblyName, module'.name, ModuleKind.Console)
        let ilModule = ilAssembly.MainModule
        visitClasses ilModule module'.classes
        ilAssembly.EntryPoint <- ilModule.Types.[1].Methods.[1]
        use assemblyStream = new MemoryStream()
        ilAssembly.Write(assemblyStream)
        { assemblyName = module'.name
          assemblyContents = assemblyStream.ToArray()
          configName = assembly.name + ".runtimeconfig.json"
          configContents = runtimeConfigJson }

    and visitClasses ilModule classes =
        let rec addClasses (classes:Class list) =
            match classes with
            | cls :: classes ->
                let ilClass = TypeDefinition(cls.namespace', cls.name, cls.attributes)
                ilModule.Types.Add(ilClass)
                addClasses classes
            | [] -> ()

        let rec addBaseClasses (classes:Class list) =
            match classes with
            | cls :: classes ->
                let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = cls.name)
                match cls.baseClassName with
                | Some baseClassName ->
                    ilClass.BaseType <- ilModule.Types.Single(fun ilType -> ilType.Name = baseClassName)
                | None -> ilClass.BaseType <- ilModule.TypeSystem.Object
                addBaseClasses classes
            | [] -> ()

        addClasses classes
        addBaseClasses classes
        visitFields ilModule classes
        visitMethods ilModule classes

    and visitFields ilModule classes =
        let rec visitFields (ilClass:TypeDefinition) (fields:Field list) =
            match fields with
            | field :: fields ->
                let ilType = visitType ilModule field.type'
                let ilField = FieldDefinition(field.name, field.attributes, ilType)
                ilClass.Fields.Add(ilField)
                visitFields ilClass fields
            | [] -> ()

        classes |> List.iter (fun cls ->
                                 let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = cls.name)
                                 visitFields ilClass cls.fields)

    and visitMethods ilModule classes =
        let rec addMethods (ilClass:TypeDefinition) = function
        | method :: methods ->
            let ilReturnType = visitType ilModule method.returnType
            let ilMethod = MethodDefinition(method.name, method.attributes, ilReturnType)
            visitParameters ilModule ilMethod method.parameters
            ilClass.Methods.Add(ilMethod)
            addMethods ilClass methods
        | [] -> ()

        let rec addMethodBodies (ilClass:TypeDefinition) (methods:Method list) =
            match methods with
            | method :: methods ->
                let ilMethod = ilClass.Methods.Single(fun ilMethod -> ilMethod.Name = method.name)
                visitVariables ilModule ilMethod method.variables
                visitMethodBody ilModule ilMethod method.body
                addMethodBodies ilClass methods
            | [] -> ()

        classes |> List.iter (fun cls ->
                                 let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = cls.name)
                                 addMethods ilClass cls.methods)
        classes |> List.iter (fun cls ->
                                 let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = cls.name)
                                 addMethodBodies ilClass cls.methods)

    and visitParameters ilModule ilMethod = function
        | param :: params' ->
            let ilType = visitType ilModule param.type'
            let ilParam = ParameterDefinition(param.name, param.attributes, ilType)
            ilMethod.Parameters.Add(ilParam)
            visitParameters ilModule ilMethod params'
        | [] -> ()

    and visitVariables ilModule ilMethod = function
        | variable :: variables ->
            let ilType = visitType ilModule variable.type'
            let ilVariable = VariableDefinition(ilType)
            ilMethod.Body.Variables.Add(ilVariable)
            visitVariables ilModule ilMethod variables
        | [] -> ()

    and visitMethodBody ilModule ilMethod = function
        | opcode :: opcodes ->
            let ilProcessor = ilMethod.Body.GetILProcessor()
            let ilInstruction = visitOpcode ilModule ilProcessor opcode
            ilProcessor.Append(ilInstruction)
            visitMethodBody ilModule ilMethod opcodes
        | [] -> ()

    and visitType ilModule = function
        | StringArrayType -> ilModule.ImportReference(typedefof<string[]>)
        | IntegerArrayType -> ilModule.ImportReference(typedefof<int[]>)
        | IntegerType -> ilModule.TypeSystem.Int32
        | BooleanType -> ilModule.TypeSystem.Boolean
        | VoidType -> ilModule.TypeSystem.Void
        | ObjectType className -> ilModule.Types.Single(fun ilType -> ilType.Name = className) :> TypeReference

    and visitOpcode ilModule ilProcessor = function
        | CallCtor className ->
            match className with
            | Some className ->
                let baseIlClass = ilModule.Types.Single(fun ilType -> ilType.Name = className)
                let baseIlCtor = baseIlClass.Methods.Single(fun ilMethod -> ilMethod.Name = ".ctor")
                ilProcessor.Create(OpCodes.Call, baseIlCtor)
            | None ->
                let baseIlCtor = typedefof<obj>.GetConstructor(Type.EmptyTypes)
                ilProcessor.Create(OpCodes.Call, ilModule.ImportReference(baseIlCtor))
        | Label label -> label.instruction
        | WriteLine ->
            let writeLineMethod = typedefof<Console>.GetMethod("WriteLine", [| typedefof<int> |])
            ilProcessor.Create(OpCodes.Call, ilModule.ImportReference(writeLineMethod))
        | Add -> ilProcessor.Create(OpCodes.Add)
        | Br label -> ilProcessor.Create(OpCodes.Br, label.instruction)
        | Brtrue label -> ilProcessor.Create(OpCodes.Brtrue, label.instruction)
        | Call (methodName, className) ->
            let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = className)
            let ilMethod = ilClass.Methods.Single(fun ilMethod -> ilMethod.Name = methodName)
            ilProcessor.Create(OpCodes.Call, ilMethod)
        | Callvirt (methodName, className) ->
            let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = className)
            let ilMethod = ilClass.Methods.Single(fun ilMethod -> ilMethod.Name = methodName)
            ilProcessor.Create(OpCodes.Callvirt, ilMethod)
        | Ceq -> ilProcessor.Create(OpCodes.Ceq)
        | Clt -> ilProcessor.Create(OpCodes.Clt)
        | Ldc_I4 value -> ilProcessor.Create(OpCodes.Ldc_I4, value)
        | Ldelem_I4 -> ilProcessor.Create(OpCodes.Ldelem_I4)
        | Ldarg index -> ilProcessor.Create(OpCodes.Ldarg, int index)
        | Ldfld (fieldName, className) ->
            let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = className)
            let ilField = ilClass.Fields.Single(fun ilField -> ilField.Name = fieldName)
            ilProcessor.Create(OpCodes.Ldfld, ilField)
        | Ldlen -> ilProcessor.Create(OpCodes.Ldlen)
        | Ldloc index -> ilProcessor.Create(OpCodes.Ldloc, int index)
        | Mul -> ilProcessor.Create(OpCodes.Mul)
        | Newarr -> ilProcessor.Create(OpCodes.Newarr, ilModule.ImportReference(typedefof<int[]>))
        | Newobj className ->
            let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = className)
            let ilCtor = ilClass.Methods.Single(fun ilMethod -> ilMethod.Name = ".ctor")
            ilProcessor.Create(OpCodes.Newobj, ilCtor)
        | Ret -> ilProcessor.Create(OpCodes.Ret)
        | Starg index -> ilProcessor.Create(OpCodes.Starg, int index)
        | Stelem_I4 -> ilProcessor.Create(OpCodes.Stelem_I4)
        | Stfld (fieldName, className) ->
            let ilClass = ilModule.Types.Single(fun ilType -> ilType.Name = className)
            let ilField = ilClass.Fields.Single(fun ilField -> ilField.Name = fieldName)
            ilProcessor.Create(OpCodes.Stfld, ilField)
        | Stloc index -> ilProcessor.Create(OpCodes.Stloc, int index)
        | Sub -> ilProcessor.Create(OpCodes.Sub)

    let generate assembly =
        try Ok (visitAssembly assembly)
        with Failure msg -> Error (sprintf "[ILBuilder] %s" msg)