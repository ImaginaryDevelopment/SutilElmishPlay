module App.Adapters.Api.Shared



open Fetch
open Fetch.Types

open Fable.Core
open Fable.Core.JsInterop

open Browser

open BReusable
open App.Adapters.Schema
open App.Adapters.Api.Schema



// maybe headers too?
type FetchParams = {
    QueryValues: Map<string, string> option
}

type FetchArgs = {
    Token: string
    Arg: FetchParams option // placeholder we'll want to be able to add to headers and such
    RelPath: string
}

let addQueryParam n v (url: string) =
    if url.Contains "?" then
        $"{url}&%s{n}=%s{v}"
    else
        $"{url}?%s{n}=%s{v}"

let addQueryValues url m =
    (url, m) ||> Map.fold (fun url k v -> url |> addQueryParam k v)

// does not prevent conflicting props against fetchArgs
let fetch fetchArgs requestProps f =
    async {
        let relUrl =
            fetchArgs.Arg
            |> Option.bind (fun a -> a.QueryValues)
            |> Option.map (addQueryValues fetchArgs.RelPath)
            |> Option.defaultValue fetchArgs.RelPath

        let! response =
            fetch (App.Adapters.Config.authConfig.ApiBase + relUrl) [
                yield! requestProps
                requestHeaders [ HttpRequestHeaders.Authorization $"Bearer %s{fetchArgs.Token}" ]
            ]
            |> Async.AwaitPromise

        return! f response
    }
    |> Async.catch

// handle get/post + query params and/or json body

let fetchText fetchArgs rp : Async<Result<_, exn>> =
    fetch fetchArgs rp (fun v -> v.text () |> Async.AwaitPromise)

let fetchJson<'t> tName fetchArgs rp : Async<Result<'t, exn>> =
    async {
        let! json = fetch fetchArgs rp (fun v -> v.json () |> Async.AwaitPromise) //fetchText fetchArgs rp

        match json with
        | Ok json ->
            printfn "%s:Parsing" tName
            // let parsed = Core.tryParse<'t> tName text
            return Ok(json :?> 't)
        | Error e -> return Error e
    }

let getMyInfo token : Async<Result<App.Adapters.Api.Schema.MyInfoResponse, exn>> =
    fetchJson<_>
        "MyInfoResponse"
        {
            Token = token
            RelPath = "/api/profile/myInfo"
            Arg = None
        }
        List.empty


[<RequireQualifiedAccess>]
type NavItemCreateType =
    | Link of parentPath: string
    | Folder



// for selecting parameters for a new acl
// server has its own max: 16
// we should ensure min is > 0
type AclRefValueArgs = {
    AclName: AclName
    SearchText: string
    Max: int option
}

let searchAclRefValues token (arv: AclRefValueArgs) =
    let fMap (resp: Schema.ApiAclSearchResponse) : Schema.AclSearchResult = { AclName = arv.AclName; Data = resp }

    fetchJson<App.Adapters.Api.Schema.ApiAclSearchResponse>
        "AclSearchResponse"
        {
            Token = token
            RelPath =
                let urlBase = $"/api/Navigation/Acls?Acl={arv.AclName}&Search={arv.SearchText}"

                match arv.Max with
                | Some i when i > 0 -> urlBase |> addQueryParam "Max" (string i)
                | _ -> urlBase
            Arg = None
        }
        List.empty
    |> Async.map (Result.map fMap)

// request to resolve a NavId's Acl Ref parameters
type NavAclInquiry = {
    AclName: AclName
    NavId: NavId
    AclType: AclParameterType
}

// for doing a lookup of the parameters in an existing acl for display
// do we need an acl resolve for a aclName-AclParameter reference
let getNavAclResolve
    token
    {
        AclName = AclName aclName
        NavId = NavId navId
        AclType = aclType
    }
    =
    match aclType with
    | AclParameterType.Reference _ ->
        fetchJson<NavAclResolveResponse>
            "NavAclResolveResponse"
            {
                Token = token
                RelPath = $"/api/Navigation/Acls?Acl=%s{aclName}&Resolve=%s{navId}"
                Arg = None
            }
            List.empty
        |> Async.map (Result.map (fun narResp -> AclName aclName, narResp))
        |> Async.map (Result.mapError Choice2Of2)
    | _ -> Async.ofValue (Error <| Choice1Of2 "Type is not a reference type")

type AclRefLookup = {
    AclName: AclName
    AclType: AclParameterType
    AclRefId: AclRefId
}

let getAclReferenceDisplay
    token
    {
        AclName = AclName aclName
        AclType = aclType
        AclRefId = AclRefId aclRefId
    }
    =
    match aclType with
    | AclParameterType.Reference _ ->
        fetchJson<NavAclResolveResponse>
            "NavAclResolveResponse"
            {
                Token = token
                RelPath = $"/api/Navigation/Acls?Acl=%s{aclName}&Reference=%s{aclRefId}"
                Arg = None
            }
            List.empty
        |> Async.map (Result.map (fun narResp -> AclName aclName, narResp))
        |> Async.map (Result.mapError Choice2Of2)
    | _ -> Async.ofValue (Error <| Choice1Of2 [| "Type is not a reference type" |])
