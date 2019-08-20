module Constants

open Types

let additionalTypesToGenerate =
    [| { _namespace = None
         name = "DebugProcAmd" }
       { _namespace = None
         name = "DebugProcArb" }
       { _namespace = None
         name = "DebugProcKhr" }
       { _namespace = None
         name = "Struct_cl_context" }
       { _namespace = None
         name = "Struct_cl_event" }
       { _namespace = None
         name = "ElementPointerTypeATI" }
       { _namespace = None
         name = "CombinerPortionNV" }
       { _namespace = None
         name = "FragmentLightParameterSGIX" }
       { _namespace = None
         name = "MapTypeNV" }
       { _namespace = None
         name = "ProgramTarget" }
       { _namespace = None
         name = "ProgramStringProperty" }
       { _namespace = None
         name = "IglooFunctionSelectSGIX" }
       { _namespace = None
         name = "IndexFunctionEXT" }
       { _namespace = None
         name = "ProgramFormat" }
       { _namespace = None
         name = "MatrixIndexPointerTypeARB" }
       { _namespace = None
         name = "ReplacementCodeTypeSUN" }
       { _namespace = None
         name = "SecondaryColorPointerTypeIBM" }
       { _namespace = None
         name = "VertexWeightPointerTypeEXT" }
       { _namespace = None
         name = "WeightPointerTypeARB" }
       { _namespace = None
         name = "VertexShaderWriteMaskEXT" } |]
    |> Set.ofArray
    |> Set.toArray

let reservedKeywords =
    [| "ref"; "object"; "string"; "event"; "params"; "base"; "in"; "type" |]
let graphicsNamespace = "OpenToolkit.Graphics"
let dummyTypesFileName = "DummyTypes"
let advancedDlSupport = "AdvancedDLSupport"
let mathematicsNamespace = "OpenToolkit.Mathematics"
let dummyTypesNamespace = graphicsNamespace + "." + "GL"
let prefixToRemove = [| "gl"; "GL_" |]
let sufixToRemove =
    [| "1"; "2"; "3"; "4"; "fv"; "ubv"; "u"; "uiv"; "ui"; "usv"; "us"; "i"; "iv";
       "f"; "b"; "d"; "dv"; "s"; "sv"; "ub"; "v" |]
    |> Array.sortByDescending (fun s -> s.Length)
let reservedKeywordsUpper =
    reservedKeywords |> Array.Parallel.map (fun k -> k.ToUpper())

let pointerTypeMappings =
    [| RefPointer; ArrayType; (function | GLint -> GLintptr | keep -> keep) |]

let functionOverloads =
    let dummyEnumGroupTy name =
        { groupName = name
          cases = Array.empty }
        |> GLenum

    let vectorOverload1 name betterName =
        functionOverloadsWith name betterName
            [| functionSignature Void
                   [| dummyEnumGroupTy "GetPName" |> typedParameterInfo "pname"
                      GLType.OpenToolkit Vector4
                      |> RefPointer
                      |> typedParameterInfo "vector" |]
               functionSignature Void
                   [| dummyEnumGroupTy "GetPName" |> typedParameterInfo "pname"
                      GLType.OpenToolkit Vector3
                      |> RefPointer
                      |> typedParameterInfo "vector" |]
               functionSignature Void
                   [| dummyEnumGroupTy "GetPName" |> typedParameterInfo "pname"
                      GLType.OpenToolkit Vector2
                      |> RefPointer
                      |> typedParameterInfo "vector" |]
               functionSignature Void
                   [| dummyEnumGroupTy "GetPName" |> typedParameterInfo "pname"
                      GLType.OpenToolkit Matrix4
                      |> RefPointer
                      |> typedParameterInfo "vector" |] |]

    [| 
       // vectorOverload1 "glGetFloatv" "GetFloat"
       // vectorOverload1 "glGetDoublev" "GetDouble"
       functionOverloadsWith "glGetQueryBufferObjecti64v"
           "GetQueryBufferObjecti64v"
           [| functionSignature Void
                  [| typedParameterInfo "id" GLuint
                     typedParameterInfo "buffer" GLuint

                     dummyEnumGroupTy "QueryObjectParameterName"
                     |> typedParameterInfo "pname"
                     typedParameterInfo "offset" GLintptr |] |]
       functionOverloadsWith "glGetQueryBufferObjectv" "GetQueryBufferObjectv"
           [| functionSignature Void
                  [| typedParameterInfo "id" GLuint
                     typedParameterInfo "buffer" GLuint

                     dummyEnumGroupTy "QueryObjectParameterName"
                     |> typedParameterInfo "pname"
                     typedParameterInfo "offset" GLintptr |] |]
       functionOverloadsWith "glGetQueryBufferObjectui64v"
           "GetQueryBufferObjectui64v"
           [| functionSignature Void
                  [| typedParameterInfo "id" GLuint
                     typedParameterInfo "buffer" GLuint

                     dummyEnumGroupTy "QueryObjectParameterName"
                     |> typedParameterInfo "pname"
                     typedParameterInfo "offset" GLintptr |] |]
       functionOverloadsWith "glGetQueryBufferObjectuiv"
           "GetQueryBufferObjectuiv"
           [| functionSignature Void
                  [| typedParameterInfo "id" GLuint
                     typedParameterInfo "buffer" GLuint

                     dummyEnumGroupTy "QueryObjectParameterName"
                     |> typedParameterInfo "pname"
                     typedParameterInfo "offset" GLintptr |] |] |]
    |> Array.Parallel.map (fun overload -> overload.expectedName, overload)
    |> Map.ofArray
