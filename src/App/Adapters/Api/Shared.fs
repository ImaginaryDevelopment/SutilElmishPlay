module App.Adapters.Api.Shared



open Fetch.Types

open Fable.Core
open Fable.Core.JsInterop

open Browser

open BReusable
open App.Schema
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

let mapUnsafeResponse f (response: Response) =
    let makeError = Array.singleton >> Choice1Of2 >> Error

    async {
        if response.Ok then
            return! f response
        else
            try
                let! e = response.text () |> Async.AwaitPromise
                let v: CoreApiError option = Core.deserialize e

                match v with
                | None -> return makeError "Api Error, Unable to read api error"
                | Some v ->
                    // sometimes status message is undefined
                    let serverEMessage =
                        [ v.StatusMessage; v.Message; v.ExceptionMessage; response.StatusText ]
                        |> List.choose Option.ofValueString
                        |> List.tryHead

                    let sc = v.StatusCode |> Option.defaultValue response.Status

                    match serverEMessage with
                    | Some(ValueString sm) ->
                        let msg = sprintf "%i:%s" sc sm
                        return makeError msg
                    | _ ->
                        let msg = sprintf "%i: %s" sc response.StatusText
                        return makeError msg
            with ex ->
                Core.warn ex.Message
                return makeError "Api Error, Failed to read api error"
    }


// does not prevent conflicting props against fetchArgs
// does not properly interpret server error responses
// or maybe just doesn't read the response in error conditions that provide more info
let fetch fetchArgs requestProps f =
    async {
        let relUrl =
            fetchArgs.Arg
            |> Option.bind (fun a -> a.QueryValues)
            |> Option.map (addQueryValues fetchArgs.RelPath)
            |> Option.defaultValue fetchArgs.RelPath

        let fullUrl = App.Adapters.Config.authConfig.ApiBase + relUrl // |> String.replace "+" "%2b"

        printfn "Fetching url: '%s'" relUrl

        let! response =
            // https://github.com/fable-compiler/fable-fetch/blob/master/src/Fetch.fs
            Fetch.fetchUnsafe fullUrl [
                yield! requestProps
                Fetch.requestHeaders [ HttpRequestHeaders.Authorization $"Bearer %s{fetchArgs.Token}" ]
            ]
            |> Async.AwaitPromise

        return! mapUnsafeResponse f response
    }
    |> Async.catch
    |> Async.map (function
        | Ok(Ok v) -> Ok v
        | Ok(Error t) -> Error t
        | Error e -> Error(Choice2Of2 e))

// handle get/post + query params and/or json body

let fetchText fetchArgs rp : Async<Result<_, ErrorType>> =
    fetch fetchArgs rp (fun v ->
        let result = v.text () |> Async.AwaitPromise
        result |> Async.map Ok)

let fetchJson<'t> tName fetchArgs rp : Async<Result<'t, ErrorType>> =
    async {
        let! json = fetch fetchArgs rp (fun v -> v.json () |> Async.AwaitPromise |> Async.map Ok) //fetchText fetchArgs rp

        match json with
        | Ok json ->
            printfn "%s:Parsing" tName
            // let parsed = Core.tryParse<'t> tName text
            return Ok(json :?> 't)
        | Error e -> return Error e
    }

let getMyInfo token : Async<Result<App.Adapters.Api.Schema.MyInfoResponse, ErrorType>> =
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
    AclParameterType: AclParameterType
    NavId: NavId
}

// for doing a lookup of the parameters in an existing acl for display
let getNavAclResolve
    token
    {
        AclName = AclName aclName
        AclParameterType = apt
        NavId = NavId navId
    }
    =
    match apt with
    | AclParameterType.Reference _ ->
        fetchJson<NavAclsResolveResponse>
            "NavAclsResolveResponse"
            {
                Token = token
                // TODO: this does not properly encode the url params
                RelPath = $"/api/Navigation/Acls?Acl=%s{aclName}&Resolve=%s{navId}"
                Arg = None
            }
            List.empty
        |> Ok
    | _ -> Error "Type is not a reference type"

let searchAdmin token text =
    // TODO: this does not properly encode the url params
    let url = $"/api/Navigation/AdminPicker"

    fetchJson<ApiAclSearchResponse>
        "adminPicker"
        {
            Token = token
            RelPath = url
            Arg =
                Some {
                    QueryValues = Some(Map["Search", text])
                }
        }
        List.empty

// TODO: this does not properly encode the url params
let getFolderAdmins token (NavId navId) =
    let url = $"/api/Navigation/AdminPicker?Folder={navId}"

    fetchJson<AdminPickerBulkResolveResponse>
        "getFolderAdmins"
        {
            Token = token
            RelPath = url
            Arg = None
        }
        List.empty
    |> Async.map (
        Result.map (fun v ->
            v.Errors
            |> Option.iter (fun e ->
                if Array.isEmpty e |> not then
                    eprintfn "Errors in bulk resolve"
                    Core.log e)

            v)
    )


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
    : Async<Result<_, ErrorType>> =

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
        |> Async.map (
            Result.bind (fun narResp ->
                // account for one or both properties being empty
                let resolved, errors =
                    narResp.Resolved, narResp.Errors |> Option.defaultValue Array.empty |> List.ofArray

                if errors |> List.isEmpty |> not then
                    errors
                    |> Seq.iter (fun e ->
                        eprintfn $"Resolve '%s{aclName}' error: '%A{e.AdditionalInfo}'"
                        Core.log e)

                resolved
                |> Option.iter (fun r -> printfn $"Resolved %s{aclName} - '%s{r.DisplayName}' from '%A{r.Reference}'")

                let result =
                    match resolved with
                    | None ->
                        errors
                        |> Seq.map (fun v -> v.Error.Message)
                        |> Array.ofSeq
                        |> Choice1Of2
                        |> Error
                    | Some r -> Ok r

                result)
        )
    | _ -> Async.ofValue (Error <| Choice1Of2 [| "Type is not a reference type" |])
