module App.Components.Root

open BReusable
open Core

open Fable.Core.JsInterop

open Sutil
open Sutil.CoreElements
open Sutil.Styling
open type Feliz.length

open App.Adapters
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Config
open App.Adapters.Api

module Handlers = App.Adapters.Html.Handlers

open App.Components.AclEditor
open App.Components.Gen
open App.Components.Gen.Icons
open Sutil.Core


[<RequireQualifiedAccess>]
type RootTabs =
    | Main // Root
    | Sub // Path
    | Editor // Edit

    static member ReParse(x: RootTabs) =
        match string x with
        | y when y = string Main -> Some RootTabs.Main
        | y when y = string Sub -> Some RootTabs.Sub
        | y when y = string Editor -> Some RootTabs.Editor
        | _ -> None

// parts of the model saved for use on refresh
type CachedState = {
    RootTab: RootTabs
    Path: string
    FocusedItemId: string
}

let stateStore: LocalStorage.IAccessor<CachedState> =
    LocalStorage.StorageAccess.CreateStorage("Root_CachedState")

let resolveAclsAccess =
    //Core.LocalStorage.StorageAccess<Map<string,string>>.CreateStorage "Root_AclsAccess"
    Core.LocalStorage.StorageAccessor<Map<string, string>, _>(
        "Root_AclsAccess",
        {
            Getter = Map.ofArray
            Setter = Map.toArray
        }
    )
    :> Core.LocalStorage.IAccessor<_>

type Model = {
    AppMode: ConfigType<string>
    NavRootState: RemoteData<NavItem[]>
    NavPathState: RemoteData<NavPathResponse>
    AclSearchResponse: RemoteData<AclSearchResponse>
    // we don't need to track not requested, in flight is implied by (value, None)
    AclResolutions: string list
    ResolvedAcls: Map<string, AclDisplay>
    AclTypeState: RemoteData<Acl[]>
    FocusedItem: NavItem option
    LastFocusedItemId: string
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
    let getAclSearchResponse x = x.AclSearchResponse
    let getAclTypes x = x.AclTypeState |> RemoteData.TryGet
    let getPath x = x.Path
    let getRootTab x = x.RootTab
    let getErrors x = x.Errors
    let getResolvedAcls x = x.ResolvedAcls

    let addError msg (x: Model) = {
        x with
            Errors = (msg, System.DateTime.Now) :: x.Errors
    }

    let addExnError title (ex: exn) x = addError $"%s{title}: %s{ex.Message}" x

[<RequireQualifiedAccess>]
type FetchReq =
    // request root or path
    | Nav of unit option
    | AclTypes
    | AclInquiry of NavAclInquiry

type FetchRes =
    | NavRoot of NavItem[]
    | NavPath of NavPathResponse
    | AclState of Acl[]
    | AclResolve of NavAclResolveResponse
    | AclSearchResolve of AclSearchResponse

[<RequireQualifiedAccess>]
type Msg =
    | FetchRequest of FetchReq
    | FetchResolve of FetchRes
    | FetchFail of string * exn

    | TabChange of RootTabs
    | EditorMsg of NavEditor.ParentMsg
    | FocusItem of NavItem
    | PathClick of string
    | PathChange of string
    | PathReq

let dummyData: NavItem[] =
    Array.ofList [
        {
            Id = "1"
            Path = "Path"
            Parent = "Parent"
            Type = "IDK"
            Name = "Name"
            Description = "Description"
            Icon = "user"
            Weight = 0
            Url = "url"
            HasUrlKey = false
            Acls = Array.empty
        }
    ]

module Commands =

    // assumes all calls will require a token
    let getResponse<'tReq, 'tData>
        (token: string)
        (fMsg: 'tData -> FetchRes)
        title
        (fAsync: string -> 'tReq -> Async<Result<'tData, ErrorType>>)
        x
        : Cmd<Msg> =
        let f x =
            async {
                let! resp = fAsync token x

                match resp with
                | Ok v -> return fMsg v |> Msg.FetchResolve
                | Error e -> return Msg.FetchFail(title, e)
            }

        Cmd.OfAsync.either f x id <| fun ex -> Msg.FetchFail(title, ex)

    let getNavRoot token =
        getResponse token FetchRes.NavRoot "NavRoot" App.Adapters.Api.getNavRoot ()

    let getNavPath (token, path) =
        getResponse token FetchRes.NavPath "NavPath" App.Adapters.Api.getNavPath path

    let getAcls token =
        getResponse token FetchRes.AclState "AclState" App.Adapters.Api.getAcls ()

    let getNavAclDisplays token req =
        getResponse token FetchRes.AclResolve "AclResolve" App.Adapters.Api.getNavAclResolve req

    let getNavAclParamSearch token req =
        getResponse token FetchRes.AclSearchResolve "AclSearchResolve" App.Adapters.Api.getAclRefValues req


// TODO: timeout to expire error messages, make sure they get logged to console on expiration
let init appMode =

    let cachedState: CachedState option =
        stateStore.TryGetValue()
        // re-parse bad serializations
        |> Option.map (fun cs -> {
            cs with
                RootTab = RootTabs.ReParse cs.RootTab |> Option.defaultValue RootTabs.Main
        })

    let (iState, cmd: Cmd<Msg>) =
        match appMode with
        | Demo -> NotRequested, Cmd.none
        | Auth token ->
            // Consider fetching last path also
            let cmd1: Cmd<Msg> = Commands.getNavRoot token
            let cmd2: Cmd<Msg> = Commands.getAcls token
            InFlight, Cmd.batch [ cmd1; cmd2 ]

    let resolvedAcls = resolveAclsAccess.TryGetValue()

    {
        AppMode = appMode
        NavRootState = if iState = InFlight then InFlight else NotRequested
        NavPathState = NotRequested
        AclSearchResponse = NotRequested
        AclTypeState = if iState = InFlight then InFlight else NotRequested
        FocusedItem = None
        LastFocusedItemId = cachedState |> Option.map (fun cs -> cs.FocusedItemId) |> Option.defaultValue ""
        Path = cachedState |> Option.map (fun cs -> cs.Path) |> Option.defaultValue ""
        Errors = List.empty
        AclResolutions = List.empty

        RootTab =
            match cachedState with
            // don't let editor be the focused tab without an item to focus
            | Some {
                       RootTab = RootTabs.Editor
                       FocusedItemId = NonValueString
                   } -> None
            | Some cs -> Some cs.RootTab
            | None -> None
            |> Option.defaultValue RootTabs.Main

        ResolvedAcls =
            resolvedAcls
            |> Option.map (
                Map.map (fun k v -> {
                    AclDisplay.DisplayName = v
                    AclDisplay.Reference = k
                })
            )
            |> Option.defaultValue Map.empty
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
            FocusedItemId = next.FocusedItem |> Option.map (fun fi -> fi.Id) |> Option.defaultValue ""
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
            match accessTokenOpt with
            | None -> None
            | Some accessToken ->
                let getEager fi =
                    fi.Acls
                    |> Seq.tryFind (fun acl -> Array.isEmpty acl.Parameters |> not)
                    |> Option.map (fun acl -> fi, acl)

                match se.Next.FocusedItem |> Option.bind getEager with
                | Some pair -> Some pair
                | _ -> None

                // kick off eager fetch of acl params
                |> Option.map (fun (fi, acl) ->
                    se.Next,
                    Some
                    <| Commands.getNavAclDisplays accessToken { NavId = fi.Id; AclName = acl.Name })

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
                        AclResolutions = x.NavId :: model.AclResolutions
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
            | ConfigType.Demo -> block "no dummy acl data" msg model
            | ConfigType.Auth accessToken ->
                printfn "Requesting Path '%s'" model.Path
                { model with AclTypeState = InFlight }, Commands.getAcls accessToken


    let fetchResolve msg (model: Model) : Model * Cmd<Msg> =
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
                        if String.isValueString model.LastFocusedItemId && Option.isNone model.FocusedItem then
                            match model.FocusedItem with
                            | None -> data |> Seq.tryFind (fun item -> item.Id = model.LastFocusedItemId)
                            | _ -> None
                        else
                            None
                        |> Option.orElse model.FocusedItem
            }

        | AclSearchResolve data ->
            justModel {
                model with
                    AclSearchResponse = RemoteData.Responded(Ok data)
            }

        | AclResolve x ->
            let nextMap =
                (model.ResolvedAcls, x.Resolved)
                ||> Seq.fold (fun m newItem -> m |> Map.add newItem.Reference newItem)

            match resolveAclsAccess.TrySetValue(nextMap |> Map.map (fun _ v -> v.DisplayName) |> Some) with
            | Ok() -> ()
            | Error e ->
                eprintfn "Failed to save map: '%A'" e
                log e

            justModel {
                model with
                    AclResolutions =
                        model.AclResolutions
                        |> List.except (x.Resolved |> Array.map (fun r -> r.Reference))
                    ResolvedAcls = nextMap
            }

        | FetchRes.NavPath data ->
            justModel {
                model with
                    NavPathState = Responded(Ok data)
                    FocusedItem =
                        if String.isValueString model.LastFocusedItemId && Option.isNone model.FocusedItem then
                            match model.FocusedItem with
                            | None -> data.Items |> Seq.tryFind (fun item -> item.Id = model.LastFocusedItemId)
                            | _ -> None
                        else
                            None
                        |> Option.orElse model.FocusedItem
            }

()

let update msg (model: Model) : Model * Cmd<Msg> =
    printfn "Root Update running: %A ('%s')" msg model.Path

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

    | Msg.EditorMsg(NavEditor.ParentMsg.AclSearchRequest _), { AclSearchResponse = InFlight } ->
        block "InFlight AclSearchRequest"
    | Msg.PathReq, { NavPathState = InFlight } -> block "InFlight PathReq"
    | Msg.PathChange _, { NavPathState = InFlight } -> block "InFlight PathChange"
    | Msg.PathClick _, { NavPathState = InFlight } -> block "InFlight PathClick"

    | Msg.TabChange v, { RootTab = y } when v = y -> block "already selected tab change"
    | Msg.EditorMsg(NavEditor.ParentMsg.Cancel), { FocusedItem = None } -> block "No FocusedItem for edit"
    | Msg.EditorMsg(NavEditor.ParentMsg.AclTypeChange NonValueString), { FocusedItem = None } ->
        block "No AclType for eager loading"
    | Msg.EditorMsg(NavEditor.ParentMsg.AclSearchRequest { SearchText = NonValueString }), _ ->
        block "Empty search found"

    | Msg.PathReq,
      {
          NavPathState = _
          Path = NonValueString _
      } -> block "PathReq NoPath"

    // actions

    // consider a confirm dialog for navigating from the editor?
    | Msg.TabChange v, _ -> justModel { model with RootTab = v }

    | Msg.EditorMsg(NavEditor.ParentMsg.Cancel), _ -> justModel { model with FocusedItem = None }

    // resolve a user's search request for parameter ids by search text for display name
    | Msg.EditorMsg(NavEditor.ParentMsg.AclSearchRequest search), _ ->
        match model.AppMode, model.FocusedItem with
        | ConfigType.Demo, _ -> block "Demo search not implemented"
        | ConfigType.Auth accessToken, Some fi ->
            let cmd = Commands.getNavAclParamSearch accessToken search

            {
                model with
                    AclSearchResponse = InFlight
            },
            cmd

        | ConfigType.Auth _, None -> block "AclSearch without Focused Item"

    // resolve an acl type's parameters if it has any existing
    | Msg.EditorMsg(NavEditor.ParentMsg.AclTypeChange v), _ ->
        match model.AppMode, model.FocusedItem with
        | ConfigType.Demo, _ -> block "AclTypeChange demo not implemented"
        | ConfigType.Auth accessToken, Some fi ->
            // what if we already have this resolved?
            let unresolved =
                fi.Acls
                |> Seq.tryFind (fun acl -> acl.Name = v)
                |> Option.map (fun acl ->
                    acl.Parameters
                    // strip already resolved items
                    |> Seq.except (model.ResolvedAcls.Keys)
                    // strip items that are already in flight
                    |> Seq.except (model.AclResolutions)
                    |> List.ofSeq)

            match unresolved with
            // we have already resolved these
            | Some [] -> justModel model
            | None
            | Some _ -> model, Commands.getNavAclDisplays accessToken { NavId = fi.Id; AclName = v }
        | ConfigType.Auth _, None -> block "AclTypeChange without Focused Item"


    | Msg.EditorMsg(NavEditor.Saved value), _ ->
        // TODO: post updated value back to api
        printfn "Saved!"
        block "Save not implemented"

    | Msg.FocusItem item, _ ->
        justModel {
            model with
                FocusedItem = Some(clone<NavItem> item)
                LastFocusedItemId = item.Id
        }

    | Msg.PathChange next, _ ->
        printfn "PathChange '%s' to '%s'" model.Path next

        justModel {
            model with
                Path = next
                RootTab = RootTabs.Sub
        }

    // responses:

    | Msg.FetchFail(title, ex), _ -> model |> MLens.addExnError title ex |> justModel

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
                    autofocus
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
                if item.Type = "Folder" then
                    tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
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
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty stripped) ]
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty item.Acls) ]
                ]
            ]
        ]

    let renderRootView title items dispatch =
        Html.div [
            Html.label [ text title ]
            // https://fontawesome.com/docs/web/setup/get-started
            Html.ul [
                for item in items do
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

    rule "div.iconColumn" [ Css.height (em 1.0); Css.width (em 1.0); Css.flexShrink 0 ]
    rule "div.buttonColumn" [ Css.height (em 1.0); Css.width (em 2.5); Css.flexShrink 0 ]
    rule ".tile .field" [ Css.marginRight (px 5) ]
    rule ".tile .field .control .box" [ Css.minWidth (px 450) ]
]

let view appMode =
    let store, dispatch = appMode |> Store.makeElmish init update ignore

    toGlobalWindow "root_model" store.Value
    // let selected : IStore<NavItem option> = Store.make( None )
    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store ]
        data_ "file" "Root"

        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay
        Bind.el (
            store |> Store.map MLens.getRootTab,
            fun rt ->
                let rootTab = {
                    Name = "Root"
                    TabClickMsg = Msg.TabChange RootTabs.Main
                    IsActive = rt = RootTabs.Main
                    Render =
                        let r data =
                            Renderers.renderRootView "" data dispatch

                        fun () ->
                            Bind.el (
                                store |> Store.map MLens.getNavRootState,
                                fun nrs ->
                                    Renderers.renderRemote "Root" nrs (FetchReq.Nav None |> Msg.FetchRequest) r dispatch
                            )
                }

                let pathTab = {
                    Name = "Path"
                    TabClickMsg = Msg.TabChange RootTabs.Sub
                    IsActive = rt = RootTabs.Sub
                    Render =
                        fun () ->
                            Html.div [
                                // display the path input properly based on current state
                                Bind.el (
                                    store |> Store.map MLens.getNavPathState,
                                    Renderers.renderPathInput store dispatch
                                )
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
                tabs
                    [
                        rootTab
                        pathTab
                        {
                            Name = "Edit"
                            TabClickMsg = Msg.TabChange RootTabs.Editor
                            IsActive = rt = RootTabs.Editor
                            Render =
                                let gfi = store |> Store.map MLens.getFocusedItem
                                let gAcl = store |> Store.map MLens.getAclTypes

                                let r: _ -> SutilElement =
                                    function
                                    | _, None
                                    | None, _ -> Html.div []
                                    | Some item, Some aclTypes ->
                                        let r =
                                            NavEditor.renderEditor {
                                                ResolvedAclParams = store |> Store.map MLens.getResolvedAcls
                                                AppMode = appMode
                                                AclTypes = aclTypes
                                                NavItem = item
                                                NavItemIconObservable = store |> Store.map MLens.getFocusedItem
                                                AclSearchResponse =
                                                    store.Value |> MLens.getAclSearchResponse |> RemoteData.TryGet
                                                DispatchParent = (Msg.EditorMsg >> dispatch)
                                            }

                                        r

                                fun () -> Bind.el2 gfi gAcl r

                        }
                    ]
                    dispatch
        )

    ]
    |> withStyle css
