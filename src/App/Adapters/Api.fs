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
    Name:string
    ParameterType: string
    MultiValue: bool option
    Searchable: bool option
    SelectableParameters: string[] option
}

type AclRef = {
    Name:string
    Parameters: string[]
}

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

type NavRootResponse = {
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
    Acls: AclRef []
}

type NavPathResponse = {
    Path: string
    Items: NavRootResponse[]
}

// maybe headers too?
type  FetchParams = {
    QueryValues: Map<string,string> option
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
    (url,m)
    ||> Map.fold(fun url k v ->
        url |> addQueryParam k v
    )

// handle get/post + query params and/or json body
let fetch fetchArgs f =
    async{
        let relUrl =
            fetchArgs.Arg
            |> Option.bind (fun a -> a.QueryValues)
            |> Option.map (addQueryValues fetchArgs.RelPath)
            |> Option.defaultValue fetchArgs.RelPath

        let! response =
            fetch (App.Adapters.Config.authConfig.ApiBase + relUrl) [
                requestHeaders [
                    HttpRequestHeaders.Authorization $"Bearer %s{fetchArgs.Token}"
                ]] |> Async.AwaitPromise
        return! f response
    }
    |> Async.catch

let fetchText fetchArgs : Async<Result<_,exn>> =
    fetch fetchArgs (fun v -> v.text() |> Async.AwaitPromise)

let fetchJson<'t> tName fetchArgs : Async<Result<'t,exn>> =
    async {
        let! text = fetchText fetchArgs
        match text with
        | Ok text ->
            printfn "%s:Parsing" tName
            let parsed = Core.tryParse<'t> tName text
            return parsed
        | Error e ->
            return Error e
    }

let getMyInfo token: Async<Result<MyInfoResponse,exn>> =
    fetchJson<_> "MyInfoResponse" {Token=token;RelPath="/api/profile/myInfo"; Arg=None}


// api/navigation/root
let getNavRoot token : Async<Result<NavRootResponse[],exn>> =
    fetchJson<_> "NavRootResponse[]" {Token=token;RelPath="/api/navigation/root"; Arg=None}

let getNavPath token (path: string) : Async<Result<NavPathResponse,exn>> =
    let path =
        if path.StartsWith "/" then
            "/api/navigation/root" + path
        else
            "/api/navigation/root/" + path
    fetchJson<NavRootResponse[]> "NavRootResponse'[]" {Token=token;RelPath=path;Arg=None}
    |> Async.map (Result.map(fun items -> {Path=path;Items=items}))


let getAcls token : Async<Result<Acl[],exn>> =
    fetchJson<_> "Acl[]" {Token=token;RelPath="/api/Navigation/Acls"; Arg=None}

// for selecting parameters for a new acl
let getAclRefValues token name search =
    fetchJson<AclSearchResponse> "AclSearchResponse" {Token=token;RelPath= $"/api/Navigation/Acls?Name={name}&search={search}"; Arg=None}

// for doing a lookup of the parameters in an existing acl for display
let getAclResolve token name objId =
    fetchJson<_> "AclResolve?" {Token=token;RelPath= $"/api/Navigation/Acl?Name={name}&resolve={objId}"; Arg=None}

let getNavAclResolve token (name:string) (objId:string) =
    fetchJson<NavAclResolveResponse> "???" {Token=token; RelPath= $"/api/navigation/acls?acl={name}&Resolve={objId}"; Arg=None}
