// purpose: hold globals that live for the app lifetime, and the triggering of observing them needs to be more carefully managed
module App.Global

open Sutil
open App.Adapters.Schema
open App.Adapters.Api.Schema

let resolvedAclLookup: IStore<ResolvedAclLookup> = Store.make Map.empty
// let aclSearchResult: IStore<

module ResolvedAclLookup =
    // does not check if the value changed, fires notification anyhow
    let addValue aclName aclRefId display : unit =
        let nextValue: ResolvedAclLookup =
            let m =
                resolvedAclLookup.Value
                |> Map.change
                    aclName
                    ((function
                     | None -> Map.empty
                     | Some m -> m)
                     >> Map.change aclRefId (function
                         | Some display -> Some display
                         | None -> Some display)
                     >> Some)

            m

        nextValue |> Store.set resolvedAclLookup

    // does not check if the value changed, fires notification anyhow
    let addValues aclName pairs : unit =
        resolvedAclLookup.Value
        |> Map.change
            aclName
            (Option.defaultValue Map.empty
             >> fun m ->
                 (m, pairs)
                 ||> Seq.fold (fun m (aclRefId, display) -> m |> Map.add aclRefId display)
                 |> Some)
        |> Store.set resolvedAclLookup

// allows one active search per acl name
let aclSearchResponse: IStore<Map<AclName, string * AclDisplay list>> =
    Store.make Map.empty

module AclSearchResponse =
    // does not support clearing out a value
    let addValue aclName value =
        let nextValue = aclSearchResponse.Value |> Map.change aclName (fun _ -> Some value)
        nextValue |> Store.set aclSearchResponse
