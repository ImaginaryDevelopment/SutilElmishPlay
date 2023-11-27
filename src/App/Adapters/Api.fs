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

[<RequireQualifiedAccess; StringEnum>]
type AclParameterType =
    | [<CompiledName("None")>] None
    | [<CompiledName("Selectable")>] Selectable
    | [<CompiledName("Reference")>] Reference

    static member ReParse(x: AclParameterType) =
        match string x with
        | y when y = string None -> Some AclParameterType.None
        | y when y = string Selectable -> Some AclParameterType.Selectable
        | y when y = string Reference -> Some AclParameterType.Reference
        | _ ->
            eprintfn "Could not parse: '%A'" x
            Option.None

    static member Troubleshoot (api: AclParameterType) (reParsed: AclParameterType option) =
        printfn "original: '%A'" api
        string AclParameterType.None |> printfn "Stringed: '%A'"
        console.log reParsed

        // can't test direct matching as passing the static value would use equality instead of literal matching

        let testOperations (v: AclParameterType) (rep: AclParameterType) =
            let hasEquality = v = rep
            let hasInequality = v <> rep |> not

            if not hasEquality then
                eprintfn "No Equality"

            if not hasInequality then
                eprintfn "No Inequality"

            match rep with
            | rep when rep = api -> eprintfn "is redundant parse?"
            | rep when rep <> api -> eprintfn "Match when no equality"
            | _ -> eprintfn "no equality and inequality"

            if Some api = reParsed then
                printfn "Redundant parse"
            else
                eprintfn "No Equality"

        match reParsed with
        | Some reParsed -> printfn "AreEqual? %A, %A = %A" api reParsed (reParsed = api)
        | Option.None -> eprintfn "Could not reParse '%A'" api

        match api, reParsed with

        | AclParameterType.None, Option.None -> eprintfn "NoParse: n"
        | AclParameterType.None, Some AclParameterType.Reference ->
            if string api = string AclParameterType.Reference then
                printfn "Parse required"
            else
                eprintfn "Uh: n-r"
                Core.log api
        // the api - None match might be a false positive
        | AclParameterType.None, Some AclParameterType.Selectable ->
            if string api = string AclParameterType.Selectable then
                printfn "Parse required"
            else
                eprintfn "Uh n-s"
                Core.log api

        | AclParameterType.Reference, Option.None -> eprintfn "NoParse: r"
        | AclParameterType.Reference, Some AclParameterType.None -> eprintfn "Uh r-n"
        | AclParameterType.Reference, Some AclParameterType.Selectable -> eprintfn "Uh r-s"

        | AclParameterType.Selectable, Option.None -> eprintfn "NoParse: s"
        | AclParameterType.Selectable, Some AclParameterType.None -> eprintfn "Uh s-n"
        | AclParameterType.Selectable, Some AclParameterType.Reference -> eprintfn "Uh s-r"

        | AclParameterType.None, Some AclParameterType.None ->
            reParsed |> Option.iter (testOperations AclParameterType.None)
        | AclParameterType.Reference, Some AclParameterType.Reference ->
            reParsed |> Option.iter (testOperations AclParameterType.Reference)
        | AclParameterType.Selectable, Some AclParameterType.Selectable ->
            reParsed |> Option.iter (testOperations AclParameterType.Selectable)

            if Some api = reParsed then
                printfn "Redundant parse"
            else
                eprintfn "No Equality"

            match reParsed with
            | Some rep when rep = api -> eprintfn "is redundant parse?"
            | Some rep when rep <> api -> printfn "Match when no equality"
            | Some rep -> eprintfn "no equality and inequality"
            | Option.None -> ()

        reParsed |> Option.defaultValue api



type Acl = {
    Name: string
    ParameterType: AclParameterType // None, Selectable, Reference
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


type NavItemType =
    | Link
    | Folder

    static member All = [ Link; Folder ]

    static member TryParse =
        function
        | y when y = string Link -> Some Link
        | y when y = string Folder -> Some Folder
        | _ -> None


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
        let! json = fetch fetchArgs rp (fun v -> v.json () |> Async.AwaitPromise) //fetchText fetchArgs rp

        match json with
        | Ok json ->
            printfn "%s:Parsing" tName
            // let parsed = Core.tryParse<'t> tName text
            return Ok(json :?> 't)
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

let private genNavUrl pathOpt =
    let b = "/api/navigation/root"

    match pathOpt with
    | Some(ValueString path) -> if path.StartsWith "/" then b + path else $"%s{b}/{path}"
    | Some _ ->
        let txt = "Bad PathOpt"
        eprintfn "%s" txt
        invalidArg "pathOpt" txt
    | None -> b

[<RequireQualifiedAccess>]
type NavItemCreateType =
    | Link of parentPath: string
    | Folder

type CreatingNavItem = {

    Path: string
    // required
    Type: NavItemCreateType
    // required
    // Name: Required<string>
    Name: string
    Description: string
    Icon: string
    Weight: int
    Enabled: bool
    Url: string
    HasUrlKey: bool
    [<CompiledName("Acls")>]
    AclRefs: AclRef[]

}

module private ApiNavInternals =

    // are we doing display name?
    type ApiNavItem = {
        Id: string
        Path: string
        Parent: string
        Type: string
        Name: string
        Description: string
        Icon: string
        Weight: int
        Enabled: bool option
        Url: string
        HasUrlKey: bool
        // [<CompiledName("Acls")>]
        Acls: AclRef[]
    }

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
        let url = Some item.Path |> genNavUrl |> String.replace "/root/Root" "/Root"
        printfn $"Attempting save to %s{url} - {item.Path}"

        async {

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
                        RequestProperties.Body(!^(Core.serialize item))
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
    let create token (cni: CreatingNavItem) =
        async {
            let url, itemType =
                match cni.Type with
                // | Folder when String.isValueString path  ->
                //     return Result.Error "Folder must be in root"
                | NavItemCreateType.Folder -> None |> genNavUrl, "Folder"
                | NavItemCreateType.Link parentPath -> Option.ofValueString parentPath |> genNavUrl, "Link"

            let item: ApiNavItem = {
                Acls = cni.AclRefs
                Description = cni.Description
                Enabled = Some cni.Enabled
                HasUrlKey = false
                Icon = cni.Icon
                Id = null
                Name = cni.Name
                Parent = null
                Path = null
                Type = itemType
                Weight = cni.Weight
                Url = cni.Url
            }

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

let searchAclRefValues token arv =
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

type NavItem = {
    Id: string
    Path: string
    Parent: string
    Type: NavItemType
    Name: string
    Description: string
    Icon: string
    Weight: int
    Enabled: bool
    Url: string
    HasUrlKey: bool
    [<CompiledName("Acls")>]
    AclRefs: AclRef[]
}

type NavPathResponse = { Path: string; Items: NavItem[] }

module private NavItemAdapters =

    // unsafe
    let ofApiNavItem (x: ApiNavInternals.ApiNavItem) : NavItem =
        let aclRefsPropName = nameof Unchecked.defaultof<NavItem>.AclRefs
        let enabledPropName = nameof Unchecked.defaultof<NavItem>.Enabled

        let b =
            Core.cloneSets x [
                aclRefsPropName, box x.Acls
                enabledPropName, box (x.Enabled |> Option.defaultValue false)
                match NavItemType.TryParse x.Type with
                | Some t -> nameof x.Type, box t
                | None -> ()
            ]
            |> box

        b :?> NavItem

    // unsafe
    let toApiNavItem (item: NavItem) : ApiNavInternals.ApiNavItem =
        let n = nameof Unchecked.defaultof<ApiNavInternals.ApiNavItem>.Acls

        let apiNavItem =
            item
            |> Core.controlledClone "NavItem -> ApiNavItem" [ n, box item.AclRefs ] [ nameof item.AclRefs ]
            |> box
            :?> ApiNavInternals.ApiNavItem

        apiNavItem

    let toNavPathResponse (x: ApiNavInternals.ApiNavPathResponse) =
        let result: NavPathResponse = {
            Path = x.Path
            Items = x.Items |> Array.map ofApiNavItem
        }

        result


module NavItems =
    let getNavRoot token () =
        ApiNavInternals.getNavRoot token ()
        |> Async.map (Result.map (Array.map NavItemAdapters.ofApiNavItem))

    let getNavPath token path =
        ApiNavInternals.getNavPath token path
        |> Async.map (Result.map NavItemAdapters.toNavPathResponse)

    let create token cni =
        ApiNavInternals.create token cni
        |> Async.map (Result.map (NavItemAdapters.ofApiNavItem))

    let save token (item: NavItem) =
        let apiNavItem = NavItemAdapters.toApiNavItem item

        ApiNavInternals.save token apiNavItem
        |> Async.map (Result.map NavItemAdapters.ofApiNavItem)
