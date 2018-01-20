namespace MiniJava

open System
open Mono.Cecil
open Mono.Cecil.Cil

type Assembly =
    { name: Identifier
      version: Version
      mainModule: Module }

and Module =
    { name: Identifier
      classes: Class list }

and Class =
    { namespace': Identifier
      name: Identifier
      baseClassName: Identifier option
      attributes: TypeAttributes
      fields: Field list
      methods: Method list }

and Field =
    { name: Identifier
      type': ILType
      attributes: FieldAttributes }

and Method =
    { name: Identifier
      returnType: ILType
      attributes: MethodAttributes
      parameters: Parameter list
      variables: Variable list
      body: Opcode list }

and Parameter =
    { name: Identifier
      type': ILType
      attributes: ParameterAttributes }

and Variable = { type': ILType }

and ILType =
    | StringArrayType
    | IntegerArrayType
    | IntegerType
    | BooleanType
    | VoidType
    | ObjectType of className: Identifier

and Label =
    { name: string
      instruction: Instruction }

and Opcode =
    | CallCtor of className: Identifier option
    | Label of Label
    | WriteLine

    /// <summary>Adds two values and pushes the result onto the evaluation stack.</summary>
    | Add
    /// <summary>Unconditionally transfers control to a target instruction.</summary>
    | Br of Label
    /// <summary>Transfers control to a target instruction if value is true, not null, or non-zero.</summary>
    | Brtrue of Label
    /// <summary>Calls the method indicated by the passed method descriptor.</summary>
    | Call of methodName: Identifier * className: Identifier
    /// <summary>Calls a late-bound method on an object, pushing the return value onto the evaluation stack.</summary>
    | Callvirt of methodName: Identifier * className: Identifier
    // Compares two values. If they are equal, the integer value 1 (int32) is pushed onto the evaluation stack; otherwise 0 (int32) is pushed onto the evaluation stack.
    | Ceq
    /// <summary>Compares two values. If the first value is less than the second, the integer value 1 (int32) is pushed onto the evaluation stack; otherwise 0 (int32) is pushed onto the evaluation stack.</summary>
    | Clt
    /// <summary>Pushes a supplied value of type int32 onto the evaluation stack as an int32.</summary>
    | Ldc_I4 of int32
    /// <summary>Loads the element with type int32 at a specified array index onto the top of the evaluation stack as an int32.</summary>
    | Ldelem_I4
    /// <summary>Loads an argument (referenced by a specified index value) onto the stack.</summary>
    | Ldarg of index: int
    /// <summary>Finds the value of a field in the object whose reference is currently on the evaluation stack.</summary>
    | Ldfld of fieldName: Identifier * className: Identifier
    /// <summary>Pushes the number of elements of a zero-based, one-dimensional array onto the evaluation stack.</summary>
    | Ldlen
    /// <summary>Loads the local variable at a specific index onto the evaluation stack.</summary>
    | Ldloc of index: int
    /// <summary>Multiplies two values and pushes the result on the evaluation stack.</summary>
    | Mul
    /// <summary>Pushes an object reference to a new zero-based, one-dimensional array whose elements are of a specific type onto the evaluation stack.</summary>
    | Newarr
    /// <summary>Creates a new object or a new instance of a value type, pushing an object reference (type O) onto the evaluation stack.</summary>
    | Newobj of className: Identifier
    /// <summary>Returns from the current method, pushing a return value (if present) from the callee's evaluation stack onto the caller's evaluation stack.</summary>
    | Ret
    /// <summary>Stores the value on top of the evaluation stack in the argument slot at a specified index.</summary>
    | Starg of index: int
    /// <summary>Replaces the array element at a given index with the int32 value on the evaluation stack.</summary>
    | Stelem_I4
    /// <summary>Replaces the value stored in the field of an object reference or pointer with a new value.</summary>
    | Stfld of fieldName: Identifier * className: Identifier
    /// <summary>Pops the current value from the top of the evaluation stack and stores it in a the local variable list at a specified index.</summary>
    | Stloc of index: int
    /// <summary>Subtracts one value from another and pushes the result onto the evaluation stack.</summary>
    | Sub