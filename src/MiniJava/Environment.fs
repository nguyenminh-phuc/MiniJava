namespace MiniJava

open System.Diagnostics

type Environment =
    { stack: Map<VariableSymbol, StackValue> list
      heap: Map<int, HeapValue>
      symbolTable: SymbolTable
      program: Program
      writeLineFunc: int -> unit
      timer: Stopwatch * int64 }

and StackValue =
    | IntegerValue of int
    | BooleanValue of bool
    | NullValue
    | ObjectPointer of pointerId: int * pointerType: Identifier
    | ArrayPointer of pointerId: int

and HeapValue =
    | ObjectValue of type': Identifier * fields: (Identifier * Map<VariableSymbol, StackValue>) list
    | ArrayValue of int[]

[<RequireQualifiedAccess>]
module GarbageCollector =

    let mark environment =
        let getPtrsInBlock block ptrs =
            block |> Map.fold (fun ptrs _ value -> match value with
                                                   | ObjectPointer (ptrId, _)
                                                   | ArrayPointer ptrId -> Set.add ptrId ptrs
                                                   | _ -> ptrs) ptrs

        let rec markVisited heap visitedPtrs ptrId =
            let visitedPtrs = Map.add ptrId true visitedPtrs
            Map.find ptrId heap |> function
            | ObjectValue (_, fields) ->
                let ptrs = fields |> List.fold (fun ptrs (_, block) -> getPtrsInBlock block ptrs) Set.empty
                ptrs |> Set.fold (fun visitedPtrs ptrId ->
                                     if not (Map.find ptrId visitedPtrs) then markVisited heap visitedPtrs ptrId
                                     else visitedPtrs) visitedPtrs
            | ArrayValue _ -> visitedPtrs

        let ptrs = environment.stack |> List.fold (fun ptrs block -> getPtrsInBlock block ptrs) Set.empty
        let visitedPtrs = Map.map (fun _ _ -> false) environment.heap
        ptrs |> Set.fold (fun visitedPtrs ptrId ->
                             if not (Map.find ptrId visitedPtrs) then markVisited environment.heap visitedPtrs ptrId
                             else visitedPtrs) visitedPtrs

    let sweep environment visitedPointers =
        let heap = visitedPointers |> Map.fold (fun heap ptrId marked ->
                                                   if not marked then Map.remove ptrId heap
                                                   else heap) environment.heap
        { environment with heap = heap }

    let collect environment =
        let visitedPtrs = mark environment
        sweep environment visitedPtrs

[<RequireQualifiedAccess>]
module Environment =

    let create symbolTable program writeLineFunc gcIntervalTimer =
        { stack = []
          heap = Map.empty
          symbolTable = symbolTable
          program = program
          writeLineFunc = writeLineFunc
          timer = Stopwatch.StartNew(), gcIntervalTimer }

    let peekStack environment =
        match environment.stack with
        | block :: _ -> block
        | [] -> failwith "The stack memory is empty"

    let popStack environment =
        match environment.stack with
        | block :: rest -> block, { environment with stack = rest }
        | [] -> failwith "The stack memory is empty"

    let pushToStack environment block =
        { environment with stack = block :: environment.stack }

    let addToHeap environment value =
        let rec findFreeId heap ptrId =
            if Map.containsKey ptrId heap then findFreeId heap (ptrId + 1)
            else ptrId

        let ptrId = findFreeId environment.heap 0
        let env = { environment with heap = environment.heap.Add (ptrId, value) }
        ptrId, env

    let getMethod environment className methodName findBaseClasses =
        let symbol = SymbolTable.findMethod environment.symbolTable className methodName findBaseClasses |> Option.get
        let (Program (_, classDecls)) = environment.program
        let (ClassDeclaration (_, _, _, methodDecls)) = classDecls |> List.find (fun (ClassDeclaration (name, _, _, _)) -> name = className)
        let decl = methodDecls |> List.find (fun (MethodDeclaration (name, _, _, _ ,_, _)) -> name = methodName)
        symbol, decl

    let findBlockValue block variableName =
        block |> Map.tryPick (fun (VariableSymbol (name, _) as variable) value ->
                                 if name = variableName then Some (variable, value) else None)

    let assignBlockValue block variableName value =
        let variable = block |> Map.findKey (fun (VariableSymbol (name, _)) _ -> name = variableName)
        block.Add (variable, value)

    let getStackValue environment variableName =
        let rec getField ptrType ignoreClass fields =
            match fields with
            | (type', block) :: rest ->
                if ignoreClass then
                    if type' = ptrType then getField ptrType false fields
                    else getField ptrType true rest
                else findBlockValue block variableName |> function
                     | Some (_, value) -> value
                     | None -> getField ptrType false rest
            | [] -> failwith "getStackValue -> getField: Unreachable code"

        let block = peekStack environment
        findBlockValue block variableName |> function
        | Some (_, value) -> value
        | None ->
            let (_, thisPtr) = findBlockValue block "this" |> Option.get
            let (ptrId, ptrType) = match thisPtr with
                                   | ObjectPointer (ptrId, ptrType) -> ptrId, ptrType
                                   | _ -> failwith "getStackValue: Unreachable code"
            Map.find ptrId environment.heap |> function
            | ObjectValue (_, fields) -> getField ptrType true fields
            | _ -> failwith "getStackValue: Unreachable code"

    let getHeapValue environment pointerId = Map.find pointerId environment.heap

    let assign environment variableName value =
        let rec replaceFields ptrType ignoreClass fields =
            match fields with
            | (type', block) :: rest ->
                if ignoreClass then
                    if type' = ptrType then replaceFields ptrType false fields
                    else (type', block) :: replaceFields ptrType true rest
                else findBlockValue block variableName |> function
                     | Some _ -> let block = assignBlockValue block variableName value
                                 (type', block) :: rest
                     | None -> (type', block) :: replaceFields ptrType false rest
            | [] -> failwith "assign -> replaceFields: Unreachable code"

        let block = peekStack environment
        findBlockValue block variableName |> function
        | Some _ -> let block = assignBlockValue block variableName value
                    let (_, env) = popStack environment
                    pushToStack env block
        | None ->
            let (_, thisPtr) = findBlockValue block "this" |> Option.get
            let (ptrId, ptrType) = match thisPtr with
                                   | ObjectPointer (ptrId, ptrType) -> ptrId, ptrType
                                   | _ -> failwith "assign: Unreachable code"
            let heap =
                Map.find ptrId environment.heap |> function
                | ObjectValue (type', oldFields) ->
                    let fields = replaceFields ptrType true oldFields
                    environment.heap.Add (ptrId, ObjectValue (type', fields))
                | _ -> failwith "assign: Unreachable code"
            { environment with heap = heap }

    let assignArray environment variableName index value =
        let rec getArrayPointer ptrType ignoreClass fields =
            match fields with
            | (type', block) :: rest ->
                if ignoreClass then
                    if type' = ptrType then getArrayPointer ptrType false fields
                    else getArrayPointer ptrType true rest
                else findBlockValue block variableName |> function
                     | Some (_, arrayPtr) -> arrayPtr
                     | None -> getArrayPointer ptrType false rest
            | [] -> failwith "assignArray -> getArrayPointer: Unreachable code"

        let assign = function
        | ArrayPointer ptrId ->
            Map.find ptrId environment.heap |> function
            | ArrayValue array -> array.[index] <- value
            | _ -> failwith "assignArray -> assign: Unreachable code"
        | _ -> failwith "assignArray -> assign: Unreachable code"

        let block = peekStack environment
        findBlockValue block variableName |> function
        | Some (_, arrayPtr) -> assign arrayPtr
        | None ->
            let (_, thisPtr) = findBlockValue block "this" |> Option.get
            let (ptrId, ptrType) = match thisPtr with
                                   | ObjectPointer (ptrId, ptrType) -> ptrId, ptrType
                                   | _ -> failwith "assignArray: Unreachable code"
            Map.find ptrId environment.heap |> function
            | ObjectValue (_, fields) -> 
                let arrayPtr = getArrayPointer ptrType true fields
                assign arrayPtr
            | _ -> failwith "assignArray: Unreachable code"
        environment

    let callGCIfRequired environment =
        let (sw, interval) = environment.timer
        if sw.ElapsedMilliseconds >= interval then
            let env = GarbageCollector.collect environment
            sw.Restart()
            { env with timer = (sw, interval) }
        else environment

    let createObject environment className =
        let rec setDefaultValues block = function
        | VariableSymbol (_, type') as field :: fields ->
            let defaultValue = match type' with
                               | ArrayType | ObjectType _ -> NullValue
                               | IntegerType -> IntegerValue 0
                               | BooleanType -> BooleanValue false
            let block = Map.add field defaultValue block
            setDefaultValues block fields
        | [] -> block

        let rec createFields fieldValues className =
            let (ClassSymbol (_, baseClassName, fields, _)) = SymbolTable.findClass environment.symbolTable className |> Option.get
            let block = setDefaultValues Map.empty fields
            let fieldValues' = (className, block)
            match baseClassName with
            | Some baseClassName -> createFields (fieldValues' :: fieldValues) baseClassName
            | None -> List.rev (fieldValues' :: fieldValues)

        let fields = createFields [] className
        let obj = ObjectValue (className, fields)
        addToHeap environment obj

    let createArray environment size =
        if size < 0 then failwithf "Cannot create an array with negative size"
        let array = ArrayValue (Array.zeroCreate size)
        addToHeap environment array