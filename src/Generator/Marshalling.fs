module Marshalling
open Types
open Util

let getMarshableFunctions (functions: PrintReadyTypedFunctionDeclaration[]) =
    functions
    |> Array.Parallel.choose(fun func ->
        let lengths =
            func.parameters
            |> Array.choose(fun s -> s.lengthParamName)
            |> Array.groupBy(function Single(_, lengthParamName) -> lengthParamName)
        if lengths.Length = 0 || lengths.Length > 1 then None
        else
            let lengthParamInfo, infos =
                lengths
                |> Array.maxBy(fun (key, values) -> values.Length)
            if infos.Length = 1 then
                maybe {
                    let (Single(dataParamName, lengthParamName)) as info = lengthParamInfo
                    let! dataParam = func.parameters |> Array.tryFind(fun p -> p.actualName = dataParamName)
                    let! _ = func.parameters |> Array.tryFind(fun p -> p.actualName = lengthParamName)
                    match dataParam.typ.typ with
                    | ArrayType _ ->
                        let functionParameters =
                            func.parameters
                            |> Array.choose(fun param ->
                                match param.actualName with
                                | name when name = lengthParamName -> None
                                | _ ->
                                    let res =
                                        { actualName = param.actualName
                                          prettyName = param.prettyName
                                          typ = param.typ } : MarshalableTypedParameterInfo
                                    Some res
                            )
                        let res =
                            { actualName = func.actualName
                              prettyName = func.prettyName
                              instanceParameters = func.parameters
                              functionParameters = functionParameters
                              lengthParamInfo = info
                              genericTypes = func.genericTypes
                              retType = func.retType } : MarshalableFunctionDeclaration
                        return! Some res
                    | _ -> return! None
                }
            else
                None
    )