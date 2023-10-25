module App.Adapters.Api

open BReusable
open Fetch
open Fetch.Types

open Fable.Core
open Fable.Core.JsInterop


open Browser

type MyInfoResponse = {
    ObjectID: string
    DisplayName: string
    FirstName: string
    LastName: string
    Email: string
    IsImperator: bool
}

type Acl = {
    Name: string
    ParameterType: string
    MultiValue: bool option
    Searchable: bool option
    SelectableParameters: string[] option
}

///
type AclRef = { Name: string; Parameters: string[] }

type AclDisplay = {
    Reference: string // guid
    DisplayName: string
}

type AclSearchResponse = {
    Search: string
    Results: AclDisplay[]
}

type NavAclResolveSubError = {
    Message: string
    StatusCode: int
    StatusMessage: string
    ExceptionType: string
    ExceptionMessage: string
}

type NavAclResolveErrorResponse = {
    Reference: string
    DisplayName: string
    AdditionalInfo: obj
    Error: NavAclResolveSubError
}

type NavAclResolveResponse = {
    Resolved: AclDisplay[]
    // ?
    Errors: AclDisplay[]
}

type NavItem = {
    Id: string
    Path: string
    Parent: string
    Type: string
    Name: string
    Description: string
    Icon: string
    Weight: int
    Url: string
    HasUrlKey: bool
    Acls: AclRef[]
}

type NavPathResponse = { Path: string; Items: NavItem[] }

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
        $"{url}&{n}={v}"
    else
        $"{url}?{n}={v}"

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
        let! text = fetchText fetchArgs rp

        match text with
        | Ok text ->
            printfn "%s:Parsing" tName
            let parsed = Core.tryParse<'t> tName text
            return parsed
        | Error e -> return Error e
    }

let getMyInfo token : Async<Result<MyInfoResponse, exn>> =
    fetchJson<_>
        "MyInfoResponse"
        {
            Token = token
            RelPath = "/api/profile/myInfo"
            Arg = None
        }
        List.empty


// api/navigation/root
let getNavRoot token () : Async<Result<NavItem[], exn>> =
    fetchJson<_>
        "NavItem[]"
        {
            Token = token
            RelPath = "/api/navigation/root"
            Arg = None
        }
        List.empty

let genNavUrl pathOpt =
    let b = "/api/navigation/root"

    match pathOpt with
    | Some(ValueString path) -> if path.StartsWith "/" then b + path else $"%s{b}/{path}"
    | Some _ ->
        let txt = "Bad PathOpt"
        eprintfn "%s" txt
        invalidArg "pathOpt" txt
    | None -> b

let getNavPath token (path: string) : Async<Result<NavPathResponse, exn>> =
    let path = genNavUrl (Some path)

    fetchJson<NavItem[]>
        "NavItem'[]"
        {
            Token = token
            RelPath = path
            Arg = None
        }
        List.empty
    |> Async.map (Result.map (fun items -> { Path = path; Items = items }))


let getAcls token () : Async<Result<Acl[], exn>> =
    fetchJson<_>
        "Acl[]"
        {
            Token = token
            RelPath = "/api/Navigation/Acls"
            Arg = None
        }
        List.empty

// for selecting parameters for a new acl
type AclRefValueArgs = { AclName: string; SearchText: string }

let getAclRefValues token arv =
    fetchJson<AclSearchResponse>
        "AclSearchResponse"
        {
            Token = token
            RelPath = $"/api/Navigation/Acls?Acl={arv.AclName}&Search={arv.SearchText}"
            Arg = None
        }
        List.empty

type NavAclInquiry = { AclName: string; NavId: string }

// for doing a lookup of the parameters in an existing acl for display
let getNavAclResolve token nar =
    fetchJson<NavAclResolveResponse>
        "NavAclResolveResponse"
        {
            Token = token
            RelPath = $"/api/Navigation/Acls?Acl={nar.AclName}&Resolve={nar.NavId}"
            Arg = None
        }
        List.empty

let save token (item: NavItem) =
    let url = Some item.Path |> genNavUrl |> String.replace "/root/Root" "/Root"
    printfn $"Attempting save to %s{url} - {item.Path}"

    async {

        let! result =
            fetchJson<NavItem>
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
                    RequestProperties.Body(!^(Core.serialize item))
                ]

        match result with
        | Ok v -> Core.log v
        | _ -> ()

        return result
    }
