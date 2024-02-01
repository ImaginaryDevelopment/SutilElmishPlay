module App.Adapters.Api.Raw

open Fable.Core

open BReusable

open App.Adapters.Api.Shared
open App.Adapters.Api.Schema.Raw

open Fetch
open Fetch.Types

open Fable.Core
open Fable.Core.JsInterop

let genNavUrl pathOpt =
    let b = "/api/navigation/root"

    match pathOpt with
    | Some(ValueString path) -> if path.StartsWith "/" then b + path else $"%s{b}/{path}"
    | Some _ ->
        let txt = "Bad PathOpt"
        eprintfn "%s" txt
        invalidArg "pathOpt" txt
    | None -> b

module ApiNavInternals =

    // are we doing display name?

    type ApiNavPathResponse = { Path: string; Items: ApiNavItem[] }
    // api/navigation/root
    let getNavRoot token () : Async<Result<ApiNavItem[], exn>> =

        fetchJson<ApiNavItem[]>
            "NavItem[]"
            {
                Token = token
                RelPath = "/api/navigation/root"
                Arg = None
            }
            List.empty
    // |> Async.map (Result.map (Array.map (NavItem.OfApiNavItem)))

    let getNavPath token (path: string) : Async<Result<ApiNavPathResponse, exn>> =
        let path = genNavUrl (Some path)

        fetchJson<ApiNavItem[]>
            "NavItem'[]"
            {
                Token = token
                RelPath = path
                Arg = None
            }
            List.empty
        |> Async.map (Result.map (fun items -> { Path = path; Items = items }))

    let save token (item: ApiNavItem) =
        let url =
            Some item.Path
            |> genNavUrl
            |> String.replace "/root/Root" "/Root"
            |> String.replace "/root/root" "/Root"

        printfn $"Attempting save to %s{url} - {item.Path}"

        async {
            let sItem = Core.serialize item

            using (Core.logGroup (Some "SaveApi"))
            <| fun _ ->
                Core.log sItem
                Core.log item

            let! result =
                fetchJson<ApiNavItem>
                    "NavItem"
                    {
                        Token = token
                        RelPath = url
                        Arg = None
                    }
                    [
                        RequestProperties.Method HttpMethod.PATCH
                        // https://github.com/fable-compiler/fable-fetch/issues/7
                        // RequestProperties.Body (!^ item)
                        RequestProperties.Body(!^(sItem))
                    ]

            match result with
            | Ok v -> Core.log v
            | _ -> ()

            return result
        }

    // link create will be a PUT
    // Name and Type
    // path must be root for folders
    // otherwise target destination for links
    let create token (item: ApiNavItem) =
        async {
            let url = genNavUrl (Option.ofValueString item.Path)
            // let item: ApiNavItem = {
            //     Acls = item.AclRefs
            //     Description = item.Description
            //     Enabled = Some item.Enabled
            //     HasUrlKey = false
            //     Icon = item.Icon
            //     Id = null
            //     Name = item.Name
            //     Parent = null
            //     Path = null
            //     Type = itemType
            //     Weight = item.Weight
            //     Url = item.Url
            // }

            // once we know the type make it fetch json
            let! (result: Result<ApiNavItem, _>) =
                fetchJson
                    (nameof ApiNavItem)
                    {
                        Token = token
                        RelPath = url
                        Arg = None
                    }
                    [
                        RequestProperties.Method HttpMethod.PUT
                        // https://github.com/fable-compiler/fable-fetch/issues/7
                        // RequestProperties.Body (!^ item)
                        RequestProperties.Body(!^(Core.serialize item))
                    ]

            return result
        }


module ApiAcls =

    let getAclTypes token () : Async<Result<ApiAcl[], exn>> =
        fetchJson<_>
            "Acl[]"
            {
                Token = token
                RelPath = "/api/Navigation/Acls"
                Arg = None
            }
            List.empty
