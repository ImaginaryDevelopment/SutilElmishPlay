// purpose: hold globals that live for the app lifetime, and the triggering of observing them needs to be more carefully managed
module App.Global

open Sutil
open App.Adapters.Schema
open App.Adapters.Api.Schema

let resolvedAclLookup: IStore<ResolvedAclLookup> = Store.make Map.empty
// let aclSearchResult: IStore<
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
