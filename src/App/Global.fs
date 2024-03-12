// purpose: hold globals that live for the app lifetime, and the triggering of observing them needs to be more carefully managed
module App.Global

open Sutil
open App.Adapters.Schema
open App.Adapters.Api.Schema
open App.Adapters.Html

let resolvedAclLookup: IStore<ResolvedAclLookup> = Store.make Map.empty

let isGroup = (=) (AclName "Allow-By-Group")
let isUser = (=) (AclName "Allow-By-User")

let makeTypeRStore (aclType: AclType) =
    resolvedAclLookup
    |> Store.mapRStore (fun v ->
        let next = v |> Map.tryFind aclType.Name |> Option.defaultValue Map.empty
        // printfn "rapRStore update %i - %A: %i" v.Count props.AclType.Name next.Count
        next)

// unused
let makeTypesRStore aclTypes =
    resolvedAclLookup
    |> Store.mapRStore (fun v ->
        (Map.empty, aclTypes)
        ||> Seq.fold (fun m (aclType: AclType) ->
            match v |> Map.tryFind aclType.Name with
            | None -> m
            | Some values -> m |> Map.add aclType values))

// managers listens to 2 acl types so far, group and user
// aclRefId could match between a user and a group - unsafe
let makeTypesUnsafeRStore aclTypes : IReadOnlyStore<Map<AclRefId, AclDisplay>> =
    resolvedAclLookup
    |> Store.mapRStore (fun v ->
        (Map.empty, aclTypes)
        ||> Seq.fold (fun m (aclType: AclType) ->
            match v |> Map.tryFind aclType.Name with
            | None -> m
            | Some values ->
                // add values map to m
                (m, values)
                ||> Seq.fold (fun m (KeyValue(aclRefId, aclDisplay)) -> m |> Map.add aclRefId aclDisplay)))

let mutable selectedItem: NavItem option = None

module ResolvedAclLookup =
    // Map<AclName, Map<AclRefId, AclDisplay>>
    let printRalDiagnostics title (m: ResolvedAclLookup) =
        use _ = Core.logGroup (Some title)
        printfn "%s: Map" title

        m
        |> Map.keys
        |> Seq.iter (fun k -> printfn "%s: %i" (AclName.getText k) <| Map.count m[k])

    // does not check if the value changed, fires notification anyhow
    let addValue aclName (display: AclDisplay) : unit =
        resolvedAclLookup.Update(fun m ->
            printRalDiagnostics "RBeforeAdd" m

            let nextValue =
                m
                |> Map.change
                    aclName
                    ((function
                     | None -> Map.empty
                     | Some m -> m)
                     >> Map.change display.Reference (function
                         | Some display -> Some display
                         | None -> Some display)
                     >> Some)

            printRalDiagnostics "RAfterAdd" nextValue
            nextValue)


    // does not check if the value changed, fires notification anyhow
    let addValues aclName displays : unit =

        resolvedAclLookup.Update(fun m ->
            printRalDiagnostics "RBeforeAdd" m

            let next =
                m
                |> Map.change
                    aclName
                    (Option.defaultValue Map.empty
                     >> fun m ->
                         (m, displays)
                         ||> Seq.fold (fun m (display: AclDisplay) -> m |> Map.add display.Reference display)
                         |> Some)

            printRalDiagnostics "RAfterAdd" next
            next)

// allows one active search per acl name
let aclSearchResponse: IStore<Map<AclName, string * AclDisplay list>> =
    Store.make Map.empty

module AclSearchResponse =
    // does not support clearing out a value
    let addValue aclName value =
        let nextValue = aclSearchResponse.Value |> Map.change aclName (fun _ -> Some value)
        nextValue |> Store.set aclSearchResponse
