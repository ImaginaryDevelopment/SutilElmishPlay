module App.Components.Root

open BReusable
open Core

open Fable.Core.JsInterop

open Sutil
open Sutil.CoreElements
open Sutil.Styling
open type Feliz.length

open App.Adapters
open App.Adapters.Schema
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Config
open App.Adapters.Api

module Handlers = App.Adapters.Html.Handlers

open App.Components.AclEditor
open App.Components.Gen
open App.Components.Gen.Icons
open Sutil.Core
open App.Adapters.Api.Shared
open App.Adapters.Api.Schema
open App.Adapters.Api.Mapped


[<RequireQualifiedAccess>]
type RootTabs =
    | Main // Root
    | Sub // Path
    | Editor // Edit
    | Creator // Create

    static member ReParse(x: RootTabs) =
        match string x with
        | y when y = string Main -> Some RootTabs.Main
        | y when y = string Sub -> Some RootTabs.Sub
        | y when y = string Editor -> Some RootTabs.Editor
        | y when y = string Creator -> Some RootTabs.Creator
        | _ -> None

// parts of the model saved for use on refresh
type CachedState = {
    RootTab: RootTabs
    Path: string
    FocusedNavId: string
}

let stateStore: LocalStorage.IAccessor<CachedState> =
    LocalStorage.StorageAccess.CreateStorage("Root_CachedState")

// root is the only one that needs to know if a lookup is already in flight
type AclRefState =
    | Requested
    // TODO: I'm quite sure the error property is not an AclDisplay
    | Response of Result<AclDisplay, NavAclResolveErrorResponse> // Choice<string,exn>>

// root is the only one that needs to know if a lookup is already in flight
type AclLookupState = AclLookup<AclRefState>


type Model = {
    AppMode: ConfigType<string>
    NavRootState: RemoteData<NavItem[]>
    NavPathState: RemoteData<App.Adapters.Api.Mapped.NavPathResponse>
    // AclSearchResponse: RemoteData<AclSearchResult>
    // list of NavIds that have had a bulk resolution request sent out
    // includes in flight and finished navIds
    // implies all Acls that are Refs are sent for resolution
    NavIdsSentForResolution: NavId Set
    // acl types, not a specific acl, or its children
    AclTypeState: RemoteData<AclType[]>
    FocusedItem: NavItem option
    LastFocusedItemId: NavId option
    RootTab: RootTabs
    // this should not be able to change while a request is in flight
    Path: string
    Errors: (string * System.DateTime) list
} with

    static member tryFindNavRootItem itemId model =
        model.NavRootState
        |> RemoteData.TryGet
        |> Option.bind (fun x -> x |> Array.tryFind (fun item -> item.Id = itemId))

    static member tryFindNavPathItem itemId model =
        model.NavPathState
        |> RemoteData.TryGet
        |> Option.bind (fun x -> x.Items |> Array.tryFind (fun item -> item.Id = itemId))

    static member tryFindNavItem itemId model =
        model
        |> Model.tryFindNavRootItem itemId
        |> Option.orElseWith (fun () -> model |> Model.tryFindNavPathItem itemId)

[<RequireQualifiedAccess>]
module private MLens =
    let getNavRootState x = x.NavRootState
    let getFocusedItem x = x.FocusedItem
    let getNavPathState x = x.NavPathState
    let getAclTypeState x = x.AclTypeState
    let getAclTypes x = x.AclTypeState |> RemoteData.TryGet
    let getPath x = x.Path
    let getRootTab x = x.RootTab
    let getErrors x = x.Errors

    let addError msg (x: Model) = {
        x with
            Errors = (msg, System.DateTime.Now) :: x.Errors
    }

    let addExnError title (ex: exn) x = addError $"%s{title}: %s{ex.Message}" x

    let addChcError title (choice: Choice<_, exn>) x =
        match choice with
        | Choice1Of2 errStr -> x |> addError $"%s{title}: %A{errStr}"
        | Choice2Of2 exn -> x |> addExnError title exn

[<RequireQualifiedAccess>]
type FetchReq =
    // request root or path
    | Nav of unit option
    | AclTypes
    | AclInquiry of NavAclInquiry

type FetchRes =
    | NavRoot of NavItem[]
    | NavPath of NavPathResponse
    | AclState of AclType[]
    | AclResolve of AclName * NavAclResolveResponse
    | AclSearchResolve of AclSearchResult
    | NavItemCreate of NavItem

[<RequireQualifiedAccess>]
type Msg =
    | FetchRequest of FetchReq
    | FetchResolve of FetchRes
    | FetchFail of string * Choice<string[], exn>

    | TabChange of RootTabs
    | EditorMsg of NavEditor.StandaloneParentMsg
    | CreatorMsg of NavCreator.ParentMsg
    | FocusItem of NavItem
    | PathClick of string
    | PathChange of string
    | PathReq

let dummyData: NavItem[] =
    Array.ofList [
        {
            Id = NavId "1"
            Path = "Path"
            Parent = "Parent"
            Type = Link
            Name = "Name"
            Description = "Description"
            Enabled = false
            Icon = "user"
            Weight = 0
            Url = "url"
            HasUrlKey = false
            AclRefs = Map.empty
        }
    ]

module Commands =

    type CommandWrap<'tData> = {
        Title: string
        Token: string
        FMsg: 'tData -> FetchRes
    }

    // assumes all calls will require a token
    let getResponse'<'tReq, 'tData, 'tError>
        commandWrap
        (fAsync: string -> 'tReq -> Async<Result<'tData, 'tError>>)
        fWrap
        reqArg
        : Cmd<Msg> =
        let f x =
            async {

                let! resp = fAsync commandWrap.Token x

                match resp with
                | Ok v -> return commandWrap.FMsg v |> Msg.FetchResolve
                | Error e -> return Msg.FetchFail(commandWrap.Title, fWrap e)
            }

        Cmd.OfAsync.either f reqArg id
        <| fun ex -> Msg.FetchFail(commandWrap.Title, Choice2Of2 ex)

    let getResponse commandWrap fAsync = getResponse' commandWrap fAsync id

    let getResponseEx commandWrap fAsync =
        getResponse' commandWrap fAsync Choice2Of2


    let getNavRoot token =
        getResponseEx
            {
                Title = "NavRoot"
                FMsg = FetchRes.NavRoot
                Token = token
            }
            App.Adapters.Api.Mapped.NavItems.getNavRoot
            ()

    let getNavPath (token, path) =
        getResponseEx
            {
                Title = "NavPath"
                FMsg = FetchRes.NavPath
                Token = token
            }
            App.Adapters.Api.Mapped.NavItems.getNavPath
            path

    let getAclTypes token =
        getResponseEx
            {
                Token = token
                FMsg = FetchRes.AclState
                Title = "AclState"
            }
            App.Adapters.Api.Mapped.getAclTypes
            ()

    let getNavAclDisplays token (req: NavAclInquiry) : Cmd<Msg> =
        let titling = "getNavAclDisplays"

        let f () =
            async {
                let! v = App.Adapters.Api.Shared.getNavAclResolve token req

                match v with
                | Ok v -> return FetchRes.AclResolve v |> Msg.FetchResolve
                | Error(Choice1Of2 e) -> return Msg.FetchFail(titling, Array.singleton e |> Choice1Of2)
                | Error(Choice2Of2 e) -> return Msg.FetchFail(titling, Choice2Of2 e)

            // return v |> Result.map (FetchRes.AclResolve >> Msg.FetchResolve)
            }

        let fFail exn : Msg = Msg.FetchFail(titling, Choice2Of2 exn)

        Cmd.OfAsync.either f () id fFail

    let getNavAclParamSearch token req =
        getResponseEx
            {
                Token = token
                FMsg = FetchRes.AclSearchResolve
                Title = "AclSearchResolve"
            }
            searchAclRefValues
            req

    let createNavItem token req =
        getResponseEx
            {
                Token = token
                FMsg = FetchRes.NavItemCreate
                Title = "NavItemCreate"
            }
            App.Adapters.Api.Mapped.NavItems.create
            req

    // Resolve a single Acl Ref value
    let getAclResolved token req =
        getResponse
            {
                Token = token
                FMsg = FetchRes.AclResolve
                Title = "AclResolve"
            }
            getAclReferenceDisplay
            req

// module Commands =
//     let resolveAclParams token nar : Cmd<Result<NavAclResolveResponse, string>> =
//         let mapError (ex: exn) = $"Save Error: {ex.Message}"

//         let f item =
//             async {
//                 let! resp = App.Adapters.Api.getNavAclResolve token item

//                 // TODO: swap item with response item, or at least compare them?
//                 return resp
//             }

//         Cmd.OfAsync.either f nar (Result.mapError mapError) (fun x -> mapError x |> Result.Error)


// TODO: timeout to expire error messages, make sure they get logged to console on expiration
let init appMode =

    let cachedState: CachedState option =
        stateStore.TryGetValue()
        // re-parse bad serializations
        |> Option.map (fun cs -> {
            cs with
                RootTab = RootTabs.ReParse cs.RootTab |> Option.defaultValue RootTabs.Main
        })

    let (aclTypeState, navRootState, cmd: Cmd<Msg>) =
        match appMode with
        | Demo ->
            Core.warn ("in demo mode")
            Responded(Ok App.Adapters.Demo.aclTypes), NotRequested, Cmd.none
        | Auth token ->
            // Consider fetching last path also
            let cmd1: Cmd<Msg> = Commands.getNavRoot token
            let cmd2: Cmd<Msg> = Commands.getAclTypes token
            InFlight, InFlight, Cmd.batch [ cmd1; cmd2 ]

    {
        AppMode = appMode
        NavRootState = navRootState
        NavPathState = NotRequested
        NavIdsSentForResolution = Set.empty
        AclTypeState = aclTypeState
        FocusedItem = None
        LastFocusedItemId =
            cachedState
            |> Option.bind (fun cs -> cs.FocusedNavId |> Option.ofValueString |> Option.map NavId)
        Path = cachedState |> Option.map (fun cs -> cs.Path) |> Option.defaultValue ""
        Errors = List.empty

        RootTab =
            match cachedState with
            // don't let editor be the focused tab without an item to focus
            | Some {
                       RootTab = RootTabs.Editor
                       FocusedNavId = NonValueString
                   } ->
                printfn "Blocking editor as root, no focused item"
                None
            | Some { RootTab = RootTabs.Main } -> Some RootTabs.Main
            | Some cs ->
                printfn "Using %A as root tab from cache " cs.RootTab
                Some cs.RootTab
            | None -> None
            |> Option.defaultValue RootTabs.Main
    },
    cmd

module SideEffects =
    type SideEffector = { Old: Model; Next: Model }

    let observeModel f (model, cmd) =
        f (model, cmd)
        model, cmd

    // ensure all commands flow through, make it harder to accidentally use old model
    let addSideEffect (f: SideEffector -> Model * Cmd<Msg> option) model (next, cmd) =
        match f { Old = model; Next = next } with
        | m, None -> m, cmd
        | m, Some cmd2 -> m, Cmd.batch [ cmd2; cmd ]

    let saveStateCache (next: Model) =
        Some {
            RootTab = next.RootTab
            Path = next.Path
            FocusedNavId =
                next.FocusedItem
                |> Option.map (fun { Id = NavId id } -> id)
                |> Option.defaultValue ""
        }
        |> stateStore.TrySetValue

    let includeError (f: Model -> Result<unit, string>) (model: Model, cmd) : Model * _ =
        match f model with
        | Error s ->
            eprintfn "Error with save state cache"
            model |> MLens.addError s, cmd
        | _ -> model, cmd


    let onPropChange fProp fEffect (values: SideEffector) : Model * Cmd<Msg> option =
        if fProp values.Old <> fProp values.Next then
            fEffect values
        else
            values.Next, None

    let onPropOptAddedOrChanged fProp fEffect values =
        let propNext = fProp values.Next

        if Option.isSome propNext then
            onPropChange fProp fEffect values
        else
            values.Next, None

    let whenFocusedItemChanges accessTokenOpt =
        onPropOptAddedOrChanged (fun m -> m.FocusedItem)
        <| fun se ->
            match accessTokenOpt, se.Next.AclTypeState with
            | None, _ -> None
            | Some accessToken, RemoteData.Responded(Ok aclTypes) ->

                // does nothing to remove already resolved params
                let getEager (fi: NavItem) =
                    if System.Object.ReferenceEquals(null, fi) then
                        failwith "fi was null"

                    if System.Object.ReferenceEquals(null, fi.AclRefs) then
                        Core.log fi.AclRefs
                        failwith "fi.AclRefs was null"

                    Core.log fi.AclRefs

                    fi
                    |> NavItem.GetRefParams aclTypes
                    |> Map.toSeq
                    |> Seq.choose (fun (aclName, (at, v)) ->
                        if Set.isEmpty v then
                            None
                        else
                            {
                                NavId = fi.Id
                                AclName = aclName
                                AclType = at.AclParamType
                            }
                            |> Some)
                    |> List.ofSeq
                    |> Option.ofValueList

                // kick off eager fetch of acl params
                match se.Next.FocusedItem |> Option.bind getEager with
                | Some pair ->
                    let cmd = pair |> List.map (Commands.getNavAclDisplays accessToken) |> Cmd.batch
                    (se.Next, Some cmd) |> Some
                | _ -> None
            | _, _ -> None
            |> Option.defaultValue (se.Next, None)
            |> fun (next, cmdOpt) -> { next with RootTab = RootTabs.Editor }, cmdOpt

let justModel m = m, Cmd.none

let block title msg (model: Model) : Model * Cmd<Msg> =
    printfn "Root blocked: %s (%A)" title msg
    model, Cmd.none

module Updates =
    let updateFetchRequest msg (model: Model) : Model * Cmd<Msg> =
        match msg with
        | FetchReq.AclInquiry x ->
            match model.AppMode with
            | ConfigType.Demo -> block "AclResolve demo not implemented" msg model
            | ConfigType.Auth accessToken ->
                {
                    model with
                        NavIdsSentForResolution = model.NavIdsSentForResolution |> Set.add x.NavId
                },
                Commands.getNavAclDisplays accessToken x
        | FetchReq.Nav(None) ->
            match model.AppMode with
            | ConfigType.Demo ->
                {
                    model with
                        NavRootState = Responded(Ok dummyData)
                },
                Cmd.none
            | ConfigType.Auth accessToken -> { model with NavRootState = InFlight }, Commands.getNavRoot accessToken

        | FetchReq.Nav(Some()) ->
            match model.AppMode with
            | ConfigType.Demo ->
                {
                    model with
                        NavPathState = Responded(Ok { Path = model.Path; Items = dummyData })
                },
                Cmd.none
            | ConfigType.Auth accessToken ->
                printfn "Requesting Path '%s'" model.Path
                { model with NavPathState = InFlight }, Commands.getNavPath (accessToken, model.Path)

        | FetchReq.AclTypes ->
            match model.AppMode with
            | ConfigType.Demo ->


                justModel {
                    model with
                        AclTypeState =
                            Responded(
                                Ok App.Adapters.Demo.aclTypes

                            )
                }

            | ConfigType.Auth accessToken ->
                printfn "Requesting Path '%s'" model.Path
                { model with AclTypeState = InFlight }, Commands.getAclTypes accessToken

    let fetchResolve msg (model: Model) : Model * Cmd<Msg> =
        printfn "Running fetchResolve: %A" (string msg |> String.truncateDisplay debug 20)

        match msg with
        | AclState data ->
            justModel {
                model with
                    AclTypeState = Responded(Ok data)
            }

        | NavRoot data ->
            justModel {
                model with
                    NavRootState = Responded(Ok data)
                    FocusedItem =
                        match model.LastFocusedItemId, model.FocusedItem with
                        | Some navId, None ->
                            match model.FocusedItem with
                            | None -> data |> Seq.tryFind (fun item -> item.Id = navId)
                            | _ -> None
                        | _ -> None
                        |> Option.orElse model.FocusedItem
            }

        | AclSearchResolve aclSearchResult ->
            aclSearchResult.Data.Results
            |> Seq.map (fun ad -> ad.Reference, ad)
            |> App.Global.ResolvedAclLookup.addValues aclSearchResult.AclName

            aclSearchResult.Data
            |> fun x -> (x.Search, x.Results |> List.ofArray)
            |> App.Global.AclSearchResponse.addValue aclSearchResult.AclName

            justModel model

        | NavItemCreate data -> justModel { model with FocusedItem = Some data }

        | AclResolve(aclName, data) ->
            data.Resolved
            |> Seq.map (fun ad -> ad.Reference, ad)
            |> App.Global.ResolvedAclLookup.addValues aclName

            model, Cmd.none

        | FetchRes.NavPath data ->
            justModel {
                model with
                    NavPathState = Responded(Ok data)
                    FocusedItem =
                        match model.LastFocusedItemId, model.FocusedItem with
                        | Some lastFocusedItemId, None ->
                            match model.FocusedItem with
                            | None -> data.Items |> Seq.tryFind (fun item -> item.Id = lastFocusedItemId)
                            | _ -> None
                        | _ -> None
                        |> Option.orElse model.FocusedItem
            }

    let updateAclType (v: AclType) model block : Model * Cmd<Msg> =
        printfn "AclType Changed to '%A'" v

        match model.AppMode, model.FocusedItem, model.AclTypeState with
        | _, _, RemoteData.Responded(Error e) -> block $"Unable to get AclTypes: %A{e}"
        | ConfigType.Demo, _, _ -> block "AclTypeChange demo not implemented"
        | ConfigType.Auth _, None, _ -> block "AclTypeChange without Focused Item"
        | ConfigType.Auth accessToken, Some fi, RemoteData.Responded(Ok aclTypes) when
            model.NavIdsSentForResolution |> Set.contains fi.Id |> not
            ->
            // what if we already have this resolved?
            let aclTypeOpt =
                fi
                |> NavItem.GetRefParams aclTypes
                |> Map.tryFind v.Name
                |> Option.filter (fun (_, p) -> Set.isEmpty p |> not)
                |> Option.map fst

            match aclTypeOpt with
            | Some aclType ->
                {
                    model with
                        NavIdsSentForResolution = model.NavIdsSentForResolution |> Set.add fi.Id
                },
                Commands.getNavAclDisplays accessToken {
                    AclType = aclType.AclParamType
                    NavId = fi.Id
                    AclName = v.Name
                }
            | _ -> justModel model
        | ConfigType.Auth _, Some _, RemoteData.Responded(Ok _) -> justModel model
        | _, _, RemoteData.InFlight -> block $"AclTypes are not yet available"
        | _, _, RemoteData.NotRequested -> block $"AclTypes have not been requested"

    let updateEditorSharedMsg (msg: NavShared.ParentMsg) model block : Model * Cmd<Msg> =
        match msg, model with
        // blocks
        | NavShared.AclTypeChange { Name = AclName NonValueString }, { FocusedItem = None } ->
            block "No AclType for eager loading"
        | NavShared.AclSearchRequest { SearchText = NonValueString }, _ -> block "Empty search found"

        // outsourcing

        // resolve an acl type's parameters if it has any existing
        | NavShared.AclTypeChange v, _
        | NavShared.AclTypeChange v, _ -> updateAclType v model block
        | NavShared.AclParamResolveRequest arl, _ ->
            match model.AppMode with
            | ConfigType.Auth token -> model, Commands.getAclResolved token arl
            | _ -> block "Demo acl resolve not implemented"

        // resolve a user's search request for parameter ids by search text for display name
        | NavShared.AclSearchRequest req, _ ->
            match model.AppMode with
            | ConfigType.Auth accessToken ->
                let cmd = Commands.getNavAclParamSearch accessToken req

                model, cmd
            | ConfigType.Demo -> block "Demo search not implemented"

    let updateEditorMsg (msg: NavEditor.StandaloneParentMsg) model block : Model * Cmd<Msg> =
        match msg, model with
        // blocks

        // outsourcing
        // this is redundant and for completeness
        | NavEditor.StandaloneParentMsg.ParentMsg msg, _ -> updateEditorSharedMsg msg model block
        // actions
        | NavEditor.StandaloneParentMsg.Cancel, _ ->

            justModel {
                model with
                    FocusedItem = None
                    RootTab = RootTabs.Main
            }
        | NavEditor.StandaloneParentMsg.Saved nextItem, { NavPathState = Responded(Ok resp) } ->
            // BUG: BUGS this does not account for any of the cases below in comments
            // what if this is a new item?
            // what if the item is NavPathState instead of in addition to NavRootState?
            resp.Items
            |> Array.tryFindIndex (fun item -> item.Id = nextItem.Id)
            |> function
                | Some ix ->
                    printfn "Found save to update"

                    {
                        model with
                            NavPathState =
                                Responded(
                                    Ok {
                                        resp with
                                            Items =
                                                resp.Items
                                                |> Array.mapi (fun i item -> if i = ix then nextItem else item)
                                    }
                                )
                            FocusedItem = None
                            RootTab = RootTabs.Main
                    }
                    |> justModel
                | None -> { model with FocusedItem = None } |> justModel
        | NavEditor.StandaloneParentMsg.Saved nextItem, _ ->
            let chc = "Save without NavPathState" |> Choice1Of2
            model |> MLens.addChcError "Root Save" chc |> justModel


()

let update msg (model: Model) : Model * Cmd<Msg> =
    printfn "Root Update running: %s ('%s')" (string msg |> String.truncateDisplay debug 20) model.Path

    let inline block title = block title msg model

    let atOpt =
        match model.AppMode with
        | ConfigType.Demo -> None
        | ConfigType.Auth accessToken -> Some accessToken

    match msg, model with
    // block actions
    | Msg.FetchRequest(FetchReq.Nav(None)), { NavRootState = InFlight } -> block "InFlight NavRoot"
    | Msg.FetchRequest(FetchReq.Nav(Some _)), { NavPathState = InFlight } -> block "InFlight NavPath"
    | Msg.FetchRequest FetchReq.AclTypes, { AclTypeState = InFlight } -> block "InFlight AclTypes"

    | Msg.PathReq, { NavPathState = InFlight } -> block "InFlight PathReq"
    | Msg.PathChange _, { NavPathState = InFlight } -> block "InFlight PathChange"
    | Msg.PathClick _, { NavPathState = InFlight } -> block "InFlight PathClick"

    | Msg.TabChange v, { RootTab = y } when v = y -> block "already selected tab change"
    | Msg.CreatorMsg(NavCreator.EditorParentMsg(NavShared.AclTypeChange { Name = AclName NonValueString })), _ ->
        block "No AclType for eager loading"

    | Msg.PathReq, { Path = NonValueString _ } -> block "PathReq NoPath"

    // outsourcing of blocks and actions
    | Msg.CreatorMsg(NavCreator.EditorParentMsg msg), _
    | Msg.EditorMsg(NavEditor.StandaloneParentMsg.ParentMsg msg), _ -> Updates.updateEditorSharedMsg msg model block

    | Msg.EditorMsg msg, _ -> Updates.updateEditorMsg msg model block

    // actions

    // consider a confirm dialog for navigating from the editor?
    | Msg.TabChange v, _ -> justModel { model with RootTab = v }

    | Msg.FocusItem item, _ ->
        justModel {
            model with
                FocusedItem = Some(clone<NavItem> item)
                RootTab = RootTabs.Editor
                LastFocusedItemId = Some item.Id
        }

    | Msg.PathChange next, _ ->
        printfn "PathChange '%s' to '%s'" model.Path next

        justModel {
            model with
                Path = next
                RootTab = RootTabs.Sub
        }
    | Msg.CreatorMsg(NavCreator.ParentMsg.CreateNavItem cni), _ ->
        match model.AppMode with
        | ConfigType.Demo -> block "Save not implemented"
        | ConfigType.Auth token ->
            let cmd: Cmd<Msg> = Commands.createNavItem token cni
            model, cmd

    // responses:

    | Msg.FetchFail("NavRoot" as title, ex), _ ->
        eprintfn "Failed NavRoot fetch: %s" title

        model
        |> MLens.addChcError title ex
        |> fun model ->
            {
                model with
                    NavPathState = Responded(Error ex)
            }
            |> justModel

    | Msg.FetchFail(title, ex), _ ->
        eprintfn "Failed fetch: %s" title
        model |> MLens.addChcError title ex |> justModel

    | Msg.FetchResolve msg, _ -> Updates.fetchResolve msg model

    // requests for remotes:

    | Msg.PathClick next, _ ->
        printfn "PathClick '%s' to '%s'" model.Path next

        match model.AppMode with
        | ConfigType.Demo -> model, Cmd.none
        | ConfigType.Auth accessToken ->
            {
                model with
                    Path = next
                    RootTab = RootTabs.Sub
                    NavPathState = InFlight
            },
            Commands.getNavPath (accessToken, model.Path)

    | Msg.PathReq, _ ->
        printfn "Requesting '%s'" model.Path

        match model.AppMode with
        | ConfigType.Demo -> model, Cmd.none
        | ConfigType.Auth accessToken ->
            printfn "Requesting '%s'" model.Path
            { model with NavPathState = InFlight }, Commands.getNavPath (accessToken, model.Path)

    | Msg.FetchRequest msg, _ -> Updates.updateFetchRequest msg model

    |> SideEffects.addSideEffect (SideEffects.whenFocusedItemChanges atOpt) model
    |> SideEffects.includeError SideEffects.saveStateCache

module Renderers =

    let renderLabeledField name value =
        Html.div [ Html.label [ text name ]; Html.pre [ text value ] ]

    let renderPathInput (model: IStore<Model>) dispatch =
        function
        | InFlight -> Html.input [ type' "text"; Attr.value model.Value.Path; Attr.disabled true ]
        | Responded _
        | NotRequested ->
            Html.div [
                Html.input [
                    type' "text"
                    Bind.attr ("value", model |> Store.map MLens.getPath)
                    Handlers.onValueChange dispatch Msg.PathChange
                ]
                bButton "Fetch" [
                    text "Fetch"
                    onClick
                        (fun _ ->
                            printfn "Dispatching pathReq"
                            dispatch Msg.PathReq)
                        List.empty
                ]
            ]

    let renderItemView (item: NavItem) (dispatch: Dispatch<Msg>) =
        let stripped = cloneExcept (item, [ "Acls" ])

        Html.divc "columns" [
            Html.divc "column is-one-fifth buttonColumn" [
                if item.Type = Folder then
                    Html.spanc "icon-text" [ tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen") ]
                    onClick (fun _ -> item.Name |> Msg.PathChange |> dispatch) List.empty
            ]
            Html.divc "column is-one-fifth buttonColumn" [
                bButton "Edit" [
                    tryIcon (App.Init.IconSearchType.MuiIcon "Edit")
                    onClick (fun _ -> item |> Msg.FocusItem |> dispatch) List.empty
                ]
            ]
            Html.divc "column is-one-fifth iconColumn" [
                tryIcon (
                    App.Init.tryFindIcon item.Icon
                    |> Option.defaultWith (fun () -> App.Init.IconSearchType.FAIcon item.Icon)
                )
            ]
            Html.divc "column" [
                Html.label [
                    text item.Name
                    if not item.Enabled then
                        Attr.className "strike"
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty stripped) ]
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty item.AclRefs) ]
                ]
            ]
        ]

    let renderRootView title items dispatch =
        Html.div [
            Html.label [ text title ]
            // https://fontawesome.com/docs/web/setup/get-started
            Html.ul [
                for (item: NavItem) in items do
                    Html.li [
                        data_ "rootItem.Icon" item.Icon
                        data_ "rootItem" (Core.serialize item)
                        renderItemView item dispatch
                    ]
            ]
        ]

    let renderRemote title rdState reqMsg okRenderer (dispatch: Dispatch<Msg>) =
        let buttonText = text title

        match rdState with
        | RemoteData.NotRequested -> bButton title [ buttonText; onClick (fun _ -> dispatch reqMsg) [] ]
        | RemoteData.InFlight -> bButton title [ Attr.disabled true; buttonText ]
        | RemoteData.Responded(Ok(data)) -> okRenderer data
        | RemoteData.Responded(Error exn) -> Html.divc "error" [ text (Core.pretty exn) ]

let css = [

    rule "label>span.info" Gen.CssRules.titleIndicator
    rule "label.strike" [ Css.textDecorationLineThrough ]
    rule ".tabContainer" [ Css.width (percent 100) ]
    rule ".fill" [ Css.width (percent 100) ]


    rule "div.iconColumn" [ Css.height (em 1.0); Css.width (em 1.0); Css.flexShrink 0 ]
    rule "div.buttonColumn" [ Css.height (em 1.0); Css.width (em 2.5); Css.flexShrink 0 ]
    rule ".tile .field" [ Css.marginRight (px 5) ]
    rule ".tile .field .control .box" [ Css.minWidth (px 450) ]
    rule "div.buttonColumn button.button" [ Css.fontSize (em 0.7) ]
]

let renderEditTab appMode store dispatch =
    let gfi = store |> Store.map MLens.getFocusedItem
    let gAcl = store |> Store.map (MLens.getAclTypes >> Option.map Seq.ofArray)

    let r: _ -> SutilElement =
        function
        | _, None
        | None, _ -> Html.div []
        | Some item, Some aclTypes ->
            printfn "Render Root Path Tab"

            let itemStore =
                store
                |> Store.chooseStore
                    "RootPathTabItem"
                    (MLens.getFocusedItem,
                     fun nextItem -> {
                         store.Value with
                             FocusedItem = nextItem
                     })
                    item

            let aclStore =
                store
                |> Store.chooseRStore
                    {
                        UseEquality = true
                        DebugTitle = None
                    }
                    (MLens.getAclTypes >> Option.map Seq.ofArray)
                    aclTypes

            let r =
                NavEditor.renderEditor {
                    AppMode = appMode
                    AclTypes = aclStore
                    NavItem = itemStore
                    EditorMode = NavEditor.EditorMode.Standalone(Msg.EditorMsg >> dispatch)
                }

            r

    Bind.el2 gfi gAcl r

let view appMode =
    let store, dispatch = appMode |> Store.makeElmish init update ignore

    toGlobalWindow "root_model" store.Value

    // let selected : IStore<NavItem option> = Store.make( None )
    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store ]
        data_ "file" "Root"

        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay
        let rt =
            store
            |> Store.mapRStore
                {
                    UseEquality = true
                    DebugTitle = None // Some "getRootTab"
                }
                MLens.getRootTab

        let mapTabStore tab =
            rt
            |> Store.mapRStore
                {
                    UseEquality = true
                    DebugTitle = None // Some $"RootTabs.%A{tab}Store"
                }
                (fun rt -> rt = tab)

        printfn "Rendering Root Tabs: %A" rt

        let rootTab = {
            Name = "Root"
            TabType = NoVariance(Enabled <| Msg.TabChange RootTabs.Main)
            IsActive = mapTabStore RootTabs.Main
            Value =
                let r data =
                    Renderers.renderRootView "" data dispatch

                printfn "Rendering root tab"

                Bind.el (
                    store |> Store.map MLens.getNavRootState,
                    fun nrs -> Renderers.renderRemote "Root" nrs (FetchReq.Nav None |> Msg.FetchRequest) r dispatch
                )
        }

        let pathTab = {
            Name = "Path"
            TabType = NoVariance(Enabled <| Msg.TabChange RootTabs.Sub)
            IsActive = mapTabStore RootTabs.Sub
            Value =
                printfn "Rendering path tab"

                Html.div [
                    // display the path input properly based on current state
                    Bind.el (store |> Store.map MLens.getNavPathState, Renderers.renderPathInput store dispatch)
                    Bind.el (
                        store |> Store.map MLens.getNavPathState,
                        (fun nps ->
                            let r (x: NavPathResponse) =
                                Renderers.renderRootView $"Sub:{x.Path}" x.Items dispatch

                            Renderers.renderRemote
                                $"Path:{store.Value.Path}"
                                nps
                                (Some() |> FetchReq.Nav |> Msg.FetchRequest)
                                r
                                dispatch)
                    )
                ]
        }

        // bulma tabs
        renderTabs
            []
            [
                rootTab
                pathTab
                {
                    Name = "Edit"
                    TabType =
                        Variance(
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (fun fi ->
                                    let v =
                                        fi.FocusedItem
                                        |> Option.map (fun _ -> TabType.Enabled <| Msg.TabChange RootTabs.Editor)
                                        |> Option.defaultValue (TabType.Disabled "is-disabled")

                                    v)
                        )
                    IsActive = mapTabStore RootTabs.Editor
                    Value = renderEditTab appMode store dispatch
                }
                {
                    Name = "Create"
                    TabType = NoVariance(Enabled <| Msg.TabChange RootTabs.Creator)
                    IsActive = mapTabStore RootTabs.Creator
                    Value =
                        let gAcl = store |> Store.map MLens.getAclTypes

                        printfn "Render Create Tab"

                        Bind.el (
                            gAcl,
                            function
                            | ValueSeqOption aclTypes ->
                                let aclTypeStore =
                                    store
                                    |> Store.chooseRStore
                                        {
                                            UseEquality = true
                                            DebugTitle = None
                                        }
                                        (MLens.getAclTypes >> Option.map Seq.ofArray)
                                        aclTypes

                                printfn "Rendering Acl Creator"

                                App.Components.NavCreator.renderAclCreator {
                                    DispatchParent = (Msg.CreatorMsg >> dispatch)
                                    AppMode = appMode
                                    AclTypes = aclTypeStore
                                    Path = store.Value.Path
                                }
                            | _ ->
                                printfn "No AclTypes, not trying creation"
                                Html.div [ text "No Acl Types found" ]

                        )

                }
            ]
            dispatch

    ]
    |> withStyle css
