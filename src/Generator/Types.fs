module Types

type GLEnum =
    { name: string
      value: string }

type GLEnumGroup =
    { groupName: string
      cases: GLEnum [] }

[<RequireQualifiedAccess>]
type GLLooseType =
    { typ: string
      group: string option }

let looseType typ group: GLLooseType =
    { typ = typ
      group = group }

type LengthParamInfo =
    | Single of dataParamName:string * lengthParamName:string

type GLParameterInfo =
    { paramName: string
      lengthParamName: LengthParamInfo option
      paramType: GLLooseType }

let parameterInfo name typ =
    { paramName = name
      lengthParamName = None
      paramType = typ }

let parameterInfoWith name typ lengthParamName =
    { paramName = name
      lengthParamName = Some lengthParamName
      paramType = typ }

type GLFunctionDeclaration =
    { funcName: string
      parameters: GLParameterInfo []
      retType: GLLooseType }

type OpenToolkitType =
    | Vector2
    | Vector3
    | Vector4
    | Vector2d
    | Vector3d
    | Vector4d
    | Matrix2
    | Matrix3
    | Matrix4
    | Matrix2d
    | Matrix3d
    | Matrix4d
    | Matrix2x3
    | Matrix2x4
    | Matrix3x2
    | Matrix3x4
    | Matrix4x2
    | Matrix4x3
    | Matrix2x3d
    | Matrix2x4d
    | Matrix3x2d
    | Matrix3x4d
    | Matrix4x2d
    | Matrix4x3d

type GLType =
    | Pointer of GLType
    | GLenum of GLEnumGroup
    | GLint
    | GLboolean
    | GLdouble
    | GLbyte
    | GLfloat
    | GLchar
    | GLcharARB
    | GLclampf
    | GLfixed
    | GLint64
    | GLint64EXT
    | GLintptr
    | GLshort
    | GLsizei
    | GLsizeiptr
    | GLubyte
    | GLuint
    | GLuint64
    | GLuint64EXT
    | GLushort
    | GLvdpauSurfaceNV
    | Void
    | GLhalfNV
    | GLbitfield
    | GLclampd
    | GLclampx
    | GLeglClientBufferEXT
    | GLeglImageOES
    | GLhandleARB
    | GLintptrARB
    | GLsizeiptrARB
    | GLsync
    | Struct_cl_context
    | Struct_cl_event
    | GLDEBUGPROC
    | GLDEBUGPROCAMD
    | GLDEBUGPROCARB
    | GLDEBUGPROCKHR
    | GLVULKANPROCNV
    | OpenToolkit of OpenToolkitType
    | RefPointer of GLType
    | StructGenericType of string
    | ArrayType of GLType
    | GLString

[<RequireQualifiedAccess>]
type TypedParameterInfo =
    { name: string
      lengthParamName: LengthParamInfo option
      typ: GLType }

let typedParameterInfo name typ: TypedParameterInfo =
    { name = name
      lengthParamName = None
      typ = typ }

let typedParameterInfoWith name typ lengthParamName: TypedParameterInfo =
    { name = name
      lengthParamName = lengthParamName
      typ = typ }

[<RequireQualifiedAccess>]
type TypedFunctionDeclaration =
    { name: string
      parameters: TypedParameterInfo []
      genericTypes: string []
      retType: GLType }

[<RequireQualifiedAccess>]
type PrintReadyTypeInfo =
    { prettyTypeName: string
      typ: GLType }

type IPrintReadyTypedParameterInfo =
    abstract actualName: string
    abstract prettyName: string
    abstract typ: PrintReadyTypeInfo
    

[<RequireQualifiedAccess>]
type PrintReadyTypedParameterInfo =
    { actualName: string
      prettyName: string
      lengthParamName: LengthParamInfo option
      typ: PrintReadyTypeInfo }

      interface IPrintReadyTypedParameterInfo with
          override self.actualName = self.actualName
          override self.prettyName = self.prettyName
          override self.typ = self.typ

type IPrintReadyTypedFunctionDeclaration =
    abstract actualName: string
    abstract prettyName: string
    abstract parameters: IPrintReadyTypedParameterInfo []
    abstract genericTypes: string []
    abstract retType: PrintReadyTypeInfo
    

[<RequireQualifiedAccess>]
type PrintReadyTypedFunctionDeclaration =
    { actualName: string
      prettyName: string
      parameters: PrintReadyTypedParameterInfo []
      genericTypes: string []
      retType: PrintReadyTypeInfo }
    
    interface IPrintReadyTypedFunctionDeclaration with
        override self.actualName = self.actualName
        override self.prettyName = self.prettyName
        override self.parameters = self.parameters |> Array.map (fun e -> e :> _)
        override self.genericTypes = self.genericTypes
        override self.retType = self.retType

[<RequireQualifiedAccess>]
type PrintReadyEnum =
    { actualName: string
      prettyName: string
      value: string }

[<RequireQualifiedAccess>]
type PrintReadyEnumGroup =
    { groupName: string
      enumCases: PrintReadyEnum [] }

[<RequireQualifiedAccess>]
type MarshalableTypedParameterInfo =
    { actualName: string
      prettyName: string
      typ: PrintReadyTypeInfo }

      interface IPrintReadyTypedParameterInfo with
          override self.actualName = self.actualName
          override self.prettyName = self.prettyName
          override self.typ = self.typ

[<RequireQualifiedAccess>]
type MarshalableFunctionDeclaration =
    { actualName: string
      prettyName: string
      instanceParameters: PrintReadyTypedParameterInfo []
      functionParameters: MarshalableTypedParameterInfo []
      lengthParamInfo: LengthParamInfo
      genericTypes: string []
      retType: PrintReadyTypeInfo }
      
      interface IPrintReadyTypedFunctionDeclaration with
          override self.actualName = self.actualName
          override self.prettyName = self.prettyName
          override self.parameters = self.functionParameters |> Array.map (fun e -> e :> _)
          override self.genericTypes = self.genericTypes
          override self.retType = self.retType

let typedFunctionDeclaration name parameters retType genericTypes : TypedFunctionDeclaration =
    { name = name
      parameters = parameters
      genericTypes = genericTypes
      retType = retType }

type DummyTypeDescriptor =
    { _namespace: string option
      name: string }

[<RequireQualifiedAccess>]
type RawOpenGLSpecificationDetails =
    { version: string
      versionNumber: decimal
      functions: string Set
      enumCases: string Set }

[<RequireQualifiedAccess>]
type ExtensionInfo =
    { name: string
      functions: string []
      enumCases: string [] }

[<RequireQualifiedAccess>]
type FunctionSignature =
    { retType: GLType
      parameters: TypedParameterInfo [] }

let functionSignature retType parameters: FunctionSignature =
    { retType = retType
      parameters = parameters }

type FunctionOverload =
    { expectedName: string
      alternativeName: string option
      overloads: FunctionSignature [] }

let functionOverloads name overloads =
    { expectedName = name
      alternativeName = None
      overloads = overloads }

let functionOverloadsWith name alternativeName overloads =
    { expectedName = name
      alternativeName = Some alternativeName
      overloads = overloads }
