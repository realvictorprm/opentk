﻿// Learn more about F// at http://fsharp.org
open Types
open SpecificationOpenGL
open Util
open Constants
open System.IO
open CommandLine

type options =
    { [<Option('i', "input", Required = true,
               HelpText = "Path to specification file.")>]
      pathToSpecificationFile: string
      [<Option('o', "output", Required = true,
               HelpText = "Path to output directory.")>]
      pathToOutputDirectory: string }

open Formatting

let autoGenerateAdditionalOverloadForType (func: PrintReadyTypedFunctionDeclaration) =
    let lengthParamsSet =
        func.parameters
        |> Array.choose(fun p -> p.lengthParamName)
        |> Set.ofArray

    let rec unwrapTyFromPointer typ =
        match typ with
        | Pointer typ -> unwrapTyFromPointer typ
        | _ -> typ
    let transformPointerTy i transformGLTypeFun typ =
        let transformPointerTy typ =
            match typ with
            | Pointer(inner) ->
                let flattenedTy = unwrapTyFromPointer inner
                let res = flattenedTy |> transformGLTypeFun
                if res = flattenedTy then typ
                else res
            | inner -> inner
        match typ with
        | Pointer(GLchar) ->
            None, GLString
        | Pointer(Pointer(GLchar)) ->
            None, GLString |> ArrayType
        | Pointer(_) when unwrapTyFromPointer typ = Void ->
            let name = 
                ("TElement" + string i)
            let inner = name |> StructGenericType
            let res = inner |> transformGLTypeFun 
            if res = inner then None, typ
            else Some name, res
        | _ -> None, transformPointerTy typ
    if lengthParamsSet.Count = 0 then
        let adjustedParameters =
            func.parameters
            |> Array.map(fun currParameter ->
                match currParameter.typ.typ with
                | Pointer(GLchar) ->
                    { currParameter with
                        PrintReadyTypedParameterInfo.typ = GLString |> FSharp.PrintReady.formatTypeInfo }
                | Pointer(Pointer(GLchar)) ->
                    { currParameter with
                        PrintReadyTypedParameterInfo.typ = GLString |> ArrayType |> FSharp.PrintReady.formatTypeInfo }
                | _ -> currParameter)
        { func with parameters = adjustedParameters } |> Array.singleton
    else
        let overloadWithMapping tyMapper =
            // Easiest solution to get
            // incrementing generic type parameter names.
            let mutable i = 1
            let adjustedParameters =
                func.parameters
                |> Array.map(fun currParameter ->
                    if lengthParamsSet.Contains currParameter.actualName then
                        let _, newParameterType =
                            currParameter.typ.typ
                            |> transformPointerTy 0 RefPointer
                        None, { currParameter with typ = newParameterType |> FSharp.PrintReady.formatTypeInfo }
                    else
                        let (genericName, newParameterType) =
                            currParameter.typ.typ 
                            |> transformPointerTy i tyMapper
                        // Yes this is a bit evil, I'm sorry but this was easiest here.
                        // without hurting performance.
                        genericName |> Option.iter(fun _ -> i <- i + 1)
                        genericName, { currParameter with typ = newParameterType |> FSharp.PrintReady.formatTypeInfo })
            let genericTypes = adjustedParameters |> Array.choose fst
            let parameters = adjustedParameters |> Array.map snd
            { func with
                genericTypes = genericTypes
                parameters = parameters }
        pointerTypeMappings
        |> Array.Parallel.map overloadWithMapping

let autoGenerateOverloadForType (func: PrintReadyTypedFunctionDeclaration) =
    let keep = func

    let injectTkType typ adjustedName expectedPointerTy =
        let name = adjustedName |> FSharp.formatNameRemovingPrefix

        let parameters =
            func.parameters
            |> Array.Parallel.map (fun param ->
                let typ =
                    match param.typ.typ with
                    | Pointer(currTy) when currTy = expectedPointerTy ->
                        typ
                        |> OpenToolkit
                        |> RefPointer
                    | typ -> typ
                { param with typ = typ |> FSharp.PrintReady.formatTypeInfo })
        // This is definitely no candidate for the pointers
        Some adjustedName,
        { func with
              parameters = parameters
              prettyName = name }

    let (|EndsWithOneOf|_|) suffixes name =
        suffixes
        |> Array.tryPick(fun suffix ->
            match name with
            | EndsWith suffix adjustedName -> Some adjustedName
            | _ -> None
        )

    // The order here is very important.
    // The longer sufixes are checked first before the shorter ones

    match func.prettyName with
    // Matrix and Vector mappings
    | EndsWith "Matrix2fv" adjustedName -> injectTkType (OpenToolkitType.Matrix2) (adjustedName + "Matrix2") GLfloat
    | EndsWith "Matrix3fv" adjustedName -> injectTkType (OpenToolkitType.Matrix3) (adjustedName + "Matrix3") GLfloat
    | EndsWith "Matrix4fv" adjustedName -> injectTkType (OpenToolkitType.Matrix4) (adjustedName + "Matrix4") GLfloat
    | EndsWith "Matrix2dv" adjustedName -> injectTkType (OpenToolkitType.Matrix2d) (adjustedName + "Matrix2") GLdouble
    | EndsWith "Matrix3dv" adjustedName -> injectTkType (OpenToolkitType.Matrix3d) (adjustedName + "Matrix3") GLdouble
    | EndsWith "Matrix4dv" adjustedName -> injectTkType (OpenToolkitType.Matrix4d) (adjustedName + "Matrix4") GLdouble
    | EndsWith "Matrix2x3fv" adjustedName -> injectTkType (OpenToolkitType.Matrix2x3) (adjustedName + "Matrix2x3") GLfloat
    | EndsWith "Matrix2x4fv" adjustedName -> injectTkType (OpenToolkitType.Matrix2x4) (adjustedName + "Matrix2x4") GLfloat
    | EndsWith "Matrix3x2fv" adjustedName -> injectTkType (OpenToolkitType.Matrix3x2) (adjustedName + "Matrix3x2") GLfloat
    | EndsWith "Matrix3x4fv" adjustedName -> injectTkType (OpenToolkitType.Matrix3x4) (adjustedName + "Matrix3x4") GLfloat
    | EndsWith "Matrix4x2fv" adjustedName -> injectTkType (OpenToolkitType.Matrix4x2) (adjustedName + "Matrix4x2") GLfloat
    | EndsWith "Matrix4x3fv" adjustedName -> injectTkType (OpenToolkitType.Matrix4x3) (adjustedName + "Matrix4x3") GLfloat
    | EndsWith "Matrix2x3dv" adjustedName -> injectTkType (OpenToolkitType.Matrix2x3d) (adjustedName + "Matrix2x3") GLdouble
    | EndsWith "Matrix2x4dv" adjustedName -> injectTkType (OpenToolkitType.Matrix2x4d) (adjustedName + "Matrix2x4") GLdouble
    | EndsWith "Matrix3x2dv" adjustedName -> injectTkType (OpenToolkitType.Matrix3x2d) (adjustedName + "Matrix3x2") GLdouble
    | EndsWith "Matrix3x4dv" adjustedName -> injectTkType (OpenToolkitType.Matrix3x4d) (adjustedName + "Matrix3x4") GLdouble
    | EndsWith "Matrix4x2dv" adjustedName -> injectTkType (OpenToolkitType.Matrix4x2d) (adjustedName + "Matrix4x2") GLdouble
    | EndsWith "Matrix4x3dv" adjustedName -> injectTkType (OpenToolkitType.Matrix4x3d) (adjustedName + "Matrix4x3") GLdouble
    | EndsWith "2dv" adjustedName -> injectTkType (OpenToolkitType.Vector2d) (adjustedName + "2") GLdouble
    | EndsWith "3dv" adjustedName -> injectTkType (OpenToolkitType.Vector3d) (adjustedName + "3") GLdouble
    | EndsWith "4dv" adjustedName -> injectTkType (OpenToolkitType.Vector4d) (adjustedName + "4") GLdouble
    | EndsWith "2fv" adjustedName -> injectTkType (OpenToolkitType.Vector2) (adjustedName + "2") GLfloat
    | EndsWith "3fv" adjustedName -> injectTkType (OpenToolkitType.Vector3) (adjustedName + "3") GLfloat
    | EndsWith "4fv" adjustedName -> injectTkType (OpenToolkitType.Vector4) (adjustedName + "4") GLfloat
    // We don't want to remove the Plural
    | EndsWith "ib" _
    | EndsWith "ts" _
    | EndsWith "ls" _
    | EndsWith "ys" _
    | EndsWith "rs" _
    | EndsWith "ed" _
    | EndsWith "es" _
    | EndsWith "ufv" _
    | EndsWith "udv" _ -> None, keep
    | EndsWithOneOf sufixToRemove adjustedName -> Some adjustedName, { func with prettyName = adjustedName }
    | _ -> None, keep

[<EntryPoint>]
let main argv =
    printfn
        "Hello World from F# and welcome to the OpenTK4.0 binding generator!"
    let result = CommandLine.Parser.Default.ParseArguments<options>(argv)

    let options =
        match result with
        | :? (Parsed<options>) as parsed -> parsed.Value
        | :? (NotParsed<options>) as notParsed ->
            failwithf "Error %A" (notParsed.Errors)
        | _ -> failwith "No options given"
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.SustainedLowLatency
    let startTime = System.Diagnostics.Stopwatch.StartNew()
    let path = options.pathToSpecificationFile
    let test = OpenGL_Specification.Load path
    let enums = Parsing.getEnumsFromSpecification test
    printfn "Enum group count: %d" enums.Length
    let enumMap =
        enums
        |> Array.Parallel.map (fun group -> group.groupName, group)
        |> Map.ofArray

    let functions = Parsing.getFunctions test
    printfn "Function count: %d" functions.Length
    let extensions = Parsing.getExtensions test
    printfn "Extension count: %d" extensions.Length
    let getMapper fn =
        extensions
        |> Array.Parallel.collect
            (fun extension ->
            fn extension
            |> Array.Parallel.map (fun value -> extension.name, value))
        |> Array.groupBy snd
        |> Array.Parallel.map
            (fun (value, extensions) ->
            value, extensions |> Array.Parallel.map fst)
        |> Map.ofArray

    let functionToExtensionMapper =
        getMapper (fun extension -> extension.functions)
    let enumCaseToExtensionMapper =
        getMapper (fun extension -> extension.enumCases)

    let typecheckedFunctions =
        TypeMapping.looslyTypedFunctionsToTypedFunctions enumMap functions
        |> Array.Parallel.collect (fun func ->            
            let func = Formatting.FSharp.PrintReady.formatTypedFunctionDeclaration func
            match functionOverloads |> Map.tryFind func.actualName with
            | Some overload ->
                overload.overloads
                |> Array.Parallel.map
                    (fun currOverload ->
                    { func with
                          prettyName =
                              overload.alternativeName
                              |> Option.defaultValue func.prettyName
                          parameters =
                              currOverload.parameters
                              |> Array.map
                                  Formatting.FSharp.PrintReady.formatTypeTypeParameterInfo
                          retType =
                              currOverload.retType
                              |> Formatting.FSharp.PrintReady.formatTypeInfo })
            | None ->
                let newName, primaryOverload =
                    func
                    |> autoGenerateOverloadForType
                let funcWithOriginalSignature =
                    newName
                    |> Option.map(fun newName -> { func with prettyName = newName })
                    |> Option.defaultValue func
                let additionalOverloads = autoGenerateAdditionalOverloadForType funcWithOriginalSignature
                [| funcWithOriginalSignature
                   primaryOverload |]
                |> Array.append additionalOverloads
                |> Array.distinct)

    let prettyEnumGroups =
        enums |> Array.Parallel.map Formatting.FSharp.PrintReady.formatEnumGroup

    let prettyEnumGroupMap =
        prettyEnumGroups
        |> Array.Parallel.map (fun group -> group.groupName, group)
        |> Map.ofArray
    printfn "overall correct function specifications: %d"
        typecheckedFunctions.Length
    let openGlVersions = Aggregator.getEnumCasesAndCommandsPerVersion test
    let basePath = options.pathToOutputDirectory

    let getShortTagForOpenGlVersion (openGl: RawOpenGLSpecificationDetails) =
        let str = sprintf "%M" openGl.versionNumber
        if openGl.version.Contains "ES" then str + "_ES"
        else str

    let getPathForOpenGlVersion (openGl: RawOpenGLSpecificationDetails) =
        basePath </> (getShortTagForOpenGlVersion openGl)
    basePath
    |> Directory.CreateDirectory
    |> ignore
    Formatting.FSharp.generateDummyTypes
    |> fun content ->
        File.WriteAllText(basePath </> dummyTypesFileName + ".fs", content)
    let inline writeToFile pathToFile topic content =
        pathToFile
        |> Directory.CreateDirectory
        |> ignore
        let fileName = topic + ".fs"
        Directory.CreateDirectory pathToFile |> ignore
        File.WriteAllText(pathToFile </> fileName, content)
        pathToFile </> fileName
    let inline writeToFileSpec (openGl: RawOpenGLSpecificationDetails) topic content =
        let pathToFile = getPathForOpenGlVersion openGl
        writeToFile pathToFile topic content

    let typecheckedFunctionsExtensionsOnly =
        typecheckedFunctions
        |> Array.filter
            (fun func -> (functionToExtensionMapper |> Map.containsKey func.actualName))
    let enumsWithExtensionsOnly =
        let requiredEnumsFromFunctions =
            typecheckedFunctionsExtensionsOnly
            |> Array.Parallel.collect
                (fun func ->
                func.parameters
                |> Array.Parallel.choose
                    (fun param ->
                    TypeMapping.tryGetEnumType param.typ.typ
                    |> Option.bind
                        (fun group ->
                        prettyEnumGroupMap |> Map.tryFind group.groupName)))
        prettyEnumGroups
        |> Array.Parallel.choose (fun enum ->
            // if enum.groupName = "ClipPlaneName" then System.Diagnostics.Debugger.Break()
            let cases =
                enum.enumCases
                |> Array.filter
                    (fun case ->
                        enumCaseToExtensionMapper
                        |> Map.containsKey case.actualName)
            if cases.Length > 0 then { enum with enumCases = cases } |> Some
            else None)
        |> Array.append requiredEnumsFromFunctions
        |> Array.distinctBy (fun e -> e.groupName)
    

    [|  let path = basePath </> "Extensions"
        yield Formatting.FSharp.generateEnums enumsWithExtensionsOnly (GenerateDetails.Extensions)
            |> writeToFile path "EnumsExtensionsOnly"

        yield Formatting.FSharp.generateInterface typecheckedFunctionsExtensionsOnly (GenerateDetails.Extensions)
            |> writeToFile path "InterfaceExtensionsOnly"

        yield Formatting.FSharp.generateStaticClass typecheckedFunctionsExtensionsOnly
                    (GenerateDetails.Extensions)
                        |> writeToFile path "StaticClassExtensionsOnly"

        yield Formatting.FSharp.generateLibraryLoaderFor (GenerateDetails.Extensions)
            |> writeToFile path "LibraryLoaderExtensionsOnly" |]
    |> ignore
    openGlVersions
    |> Array.Parallel.collect (fun glVersion ->
        let inline writeToFile topic content =
            writeToFileSpec glVersion topic content
        let typecheckedFunctionsWithoutExtensions =
            typecheckedFunctions
            |> Array.filter
                (fun func ->
                glVersion.functions.Contains func.actualName
                && (functionToExtensionMapper |> Map.containsKey func.actualName |> not))

        let enumsWithoutExtensions =
            let requiredEnumsFromFunctions =
                typecheckedFunctionsWithoutExtensions
                |> Array.Parallel.collect
                    (fun func ->
                    func.parameters
                    |> Array.Parallel.choose
                        (fun param ->
                        TypeMapping.tryGetEnumType param.typ.typ
                        |> Option.bind
                            (fun group ->
                            prettyEnumGroupMap |> Map.tryFind group.groupName)))
            prettyEnumGroups
            |> Array.Parallel.choose (fun enum ->
                // if enum.groupName = "ClipPlaneName" then System.Diagnostics.Debugger.Break()
                let cases =
                    enum.enumCases
                    |> Array.filter
                        (fun case ->
                        glVersion.enumCases.Contains case.actualName
                        && (enumCaseToExtensionMapper
                            |> Map.containsKey case.actualName
                            |> not))
                if cases.Length > 0 then { enum with enumCases = cases } |> Some
                else None)
            |> Array.append requiredEnumsFromFunctions
            |> Array.distinctBy (fun e -> e.groupName)
                

        let generatedFiles =
            [| yield Formatting.FSharp.generateEnums enumsWithoutExtensions (GenerateDetails.OpenGlVersion glVersion)
                    |> writeToFile "EnumsWithoutExtensions"

               yield Formatting.FSharp.generateInterface typecheckedFunctionsWithoutExtensions (GenerateDetails.OpenGlVersion glVersion)
                    |> writeToFile "InterfaceWithoutExtensions"

               yield Formatting.FSharp.generateStaticClass typecheckedFunctionsWithoutExtensions
                         (GenerateDetails.OpenGlVersion glVersion)
                                |> writeToFile "StaticClassWithoutExtensions"

               yield Formatting.FSharp.generateLibraryLoaderFor (GenerateDetails.OpenGlVersion glVersion)
                    |> writeToFile "LibraryLoaderWithoutExtensions" |]

        printfn "Done writing OpenGL Version %s files." glVersion.version
        generatedFiles)
    |> ignore
    printfn "Generating files took %s seconds"
        (startTime.Elapsed.Seconds |> string)
    0 // return an integer exit code
