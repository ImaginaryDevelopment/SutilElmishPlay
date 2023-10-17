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

type RootTabs =
    | RootMain
    | RootSub
    | RootEditor

type Model = {
    AppMode: ConfigType<string>
    NavRootState : RemoteData<NavRootResponse[]>
    NavPathState : RemoteData<NavPathResponse>
    AclTypeState : RemoteData<Acl[]>
    FocusedItem : NavRootResponse option
    RootTab: RootTabs
    // this should not be able to change while a request is in flight
    Path: string
}
    with
        static member tryFindNavRootItem itemId model =
            model.NavRootState |> RemoteData.TryGet |> Option.bind(fun x -> x |> Array.tryFind(fun item -> item.Id = itemId))
        static member tryFindNavPathItem itemId model =
            model.NavPathState |> RemoteData.TryGet |> Option.bind(fun x -> x.Items |> Array.tryFind(fun item -> item.Id = itemId))
        static member tryFindNavItem itemId model =
            model |> Model.tryFindNavRootItem itemId
            |> Option.orElseWith(fun () ->
                model |> Model.tryFindNavPathItem itemId
            )

[<RequireQualifiedAccess>]
module MLens =
    let getNavRootState x = x.NavRootState
    let getFocusedItem x = x.FocusedItem
    let getNavPathState x = x.NavPathState
    let getAclTypeState x = x.AclTypeState
    let getAclTypes x = x.AclTypeState |> RemoteData.TryGet
    let getPath x = x.Path
    let getRootTab x = x.RootTab

type Msg =
    | NavRootMsg of RemoteMsg<unit, NavRootResponse[]>
    | NavPathMsg of RemoteMsg<unit, NavPathResponse>
    | AclStateMsg of RemoteMsg<unit, Acl[]>
    | TabChange of RootTabs
    | EditorMsg of NavEditor.ParentMsg
    | FocusItem of NavRootResponse
    | PathClick of string
    | PathChange of string
    | PathReq

let dummyData: NavRootResponse[] =
    Array.ofList [
        {
            Id="1"
            Path="Path"
            Parent="Parent"
            Type="IDK"
            Name="Name"
            Description="Description"
            Icon="user"
            Weight=0
            Url="url"
            HasUrlKey= false
            Acls= Array.empty
        }
    ]

module Commands =
    let getNavRoot token =
        let a () =
            async {
                let! resp = App.Adapters.Api.getNavRoot token
                let resp2 = Response resp
                return Msg.NavRootMsg resp2
            }
        Cmd.OfAsync.perform a () id

    let getNavPath (token,path) =
        let a () =
            async {
                printfn "Fetching Path '%s'" path
                let! resp = App.Adapters.Api.getNavPath token path
                let resp2 = Response resp // |> Result.map(fun items -> path,items) |> Response
                return Msg.NavPathMsg resp2
            }
        Cmd.OfAsync.perform a () id

    let getAcls token =
        let a () =
            async {
                let! resp = App.Adapters.Api.getAcls token
                let resp2 = Response resp // |> Result.map(fun items -> path,items) |> Response
                return Msg.AclStateMsg resp2
            }
        Cmd.OfAsync.perform a () id

let init appMode =
    let (iState, cmd: Cmd<Msg>) = 
        match appMode with
        | Demo -> NotRequested,Cmd.none
        | Auth token ->
            let cmd1:Cmd<Msg> = Commands.getNavRoot token
            let cmd2:Cmd<Msg> = Commands.getAcls token
            InFlight, Cmd.batch [ cmd1;cmd2 ]

    {   AppMode = appMode;
        NavRootState = if iState = InFlight then InFlight else NotRequested
        NavPathState = NotRequested
        AclTypeState = if iState = InFlight then InFlight else NotRequested
        FocusedItem = None
        RootTab= RootTabs.RootMain
        Path = ""}, cmd

let update msg (model:Model) : Model * Cmd<Msg> =
    printfn "Root Update running: %A ('%s')" msg model.Path
    let block title =
        printfn "Blocked: %s (%A)" title msg
        model, Cmd.none

    match msg, model with
    // block actions
    | NavRootMsg (Request _), {NavRootState= InFlight} -> block "InFlight NavRootMsg"
    | NavPathMsg (Request _), {NavPathState= InFlight} -> block "InFlight NavPathMsg"
    | AclStateMsg (Request _), {AclTypeState= InFlight} -> block "InFlight AclStateMsg"
    | PathReq, {NavPathState= InFlight} -> block "InFlight PathReq"
    | PathChange _, {NavPathState=InFlight} -> block "InFlight PathChange"
    | PathClick _, {NavPathState=InFlight} -> block "InFlight PathClick"

    | PathReq, {NavPathState= _; Path= NonValueString _} -> block "PathReq NoPath"
    | TabChange v, {RootTab= y} when v = y -> block "already selected tab change"
    | EditorMsg(NavEditor.ParentMsg.Cancel), {FocusedItem = None} -> block "No FocusedItem for edit"


    // actions

    // consider a confirm dialog for navigating from the editor?
    | TabChange v, _ -> {model with RootTab= v}, Cmd.none

    | Msg.EditorMsg (NavEditor.ParentMsg.Cancel), _ -> {model with FocusedItem = None}, Cmd.none

    | Msg.EditorMsg (NavEditor.Saved value), _ ->
        // TODO: post updated value back to api
        let runSave () =
            model, Cmd.none

        match model |> Model.tryFindNavItem value.Id with
        | None ->
            eprintfn "Item to update not found: '%A' ('%A')" value.Id value.Name
            // TODO: report error up to user, although this case should never happen
            model, Cmd.none
        | Some _old -> // disable saves and save button, don't navigate away while we try to save
            runSave()
    | FocusItem item, _ ->
        {model with RootTab= RootTabs.RootEditor; FocusedItem = Some (clone<NavRootResponse> item)}, Cmd.none

    | PathChange next, _ ->
        printfn "PathChange '%s' to '%s'" model.Path next
        {model with Path= next; RootTab= RootTabs.RootSub}, Cmd.none

    // responses:

    | AclStateMsg (Response x), _ -> {model with AclTypeState= Responded x}, Cmd.none
    | NavRootMsg (Response x ), _ -> {model with NavRootState= Responded x}, Cmd.none

    | NavPathMsg (Response x), _ ->
        {model with NavPathState= Responded x}, Cmd.none

    // requests for remotes:

    | PathClick next, _ ->
        printfn "PathClick '%s' to '%s'" model.Path next
        match model.AppMode with
        | ConfigType.Demo -> model, Cmd.none
        | ConfigType.Auth accessToken ->
            {model with Path= next; RootTab= RootTabs.RootSub; NavPathState=InFlight}, Commands.getNavPath(accessToken, model.Path)

    | PathReq, _ ->
        printfn "Requesting '%s'" model.Path
        match model.AppMode with
        | ConfigType.Demo -> model, Cmd.none
        | ConfigType.Auth accessToken ->
            printfn "Requesting '%s'" model.Path
            {model with NavPathState= InFlight}, Commands.getNavPath(accessToken, model.Path)

    | NavRootMsg (Request _), _ ->
        match model.AppMode with
        | ConfigType.Demo ->
            {model with NavRootState=Responded (Ok dummyData)}, Cmd.none
        | ConfigType.Auth accessToken ->
            {model with NavRootState= InFlight}, Commands.getNavRoot accessToken

    | NavPathMsg (Request ()), _ ->
        match model.AppMode with
        | ConfigType.Demo ->
            {model with NavPathState= Responded(Ok {Path= model.Path; Items= dummyData})}, Cmd.none
        | ConfigType.Auth accessToken ->
            printfn "Requesting Path '%s'" model.Path
            {model with NavPathState= InFlight}, Commands.getNavPath (accessToken, model.Path)

    | AclStateMsg (Request ()), _ ->
        match model.AppMode with
        | ConfigType.Demo -> block "no dummy acl data"
        | ConfigType.Auth accessToken ->
            printfn "Requesting Path '%s'" model.Path
            {model with AclTypeState= InFlight}, Commands.getAcls accessToken

module Renderers =

    let renderLabeledField name value =
        Html.div [ Html.label [text name]; Html.pre [text value]]

    let renderPathInput (model: IStore<Model>) dispatch =
        function
        | InFlight ->
            Html.input [
                type' "text"
                Attr.value model.Value.Path
                Attr.disabled true
            ]
        | Responded _
        | NotRequested  ->
            Html.div [

                Html.input [
                    type' "text"
                    autofocus
                    Bind.attr ("value", model |> Store.map MLens.getPath)
                    Handlers.onValueChange dispatch PathChange
                ]
                Html.button [
                    text "Fetch"
                    onClick(fun _ -> printfn "Dispatching pathReq"; dispatch Msg.PathReq) List.empty
                ]
            ]

    let renderItemView (item:NavRootResponse) (dispatch: Dispatch<Msg>) =  
        let stripped = cloneExcept(item, ["Acls"])
        Html.divc "columns" [
            Html.divc "column is-one-fifth buttonColumn" [
                if item.Type = "Folder" then
                    tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
                    onClick (fun _ -> item.Name |> PathChange |> dispatch) List.empty

            ]
            Html.divc "column is-one-fifth buttonColumn" [
                Html.button [
                    tryIcon (App.Init.IconSearchType.MuiIcon "Edit")
                    onClick (fun _ -> item |> Msg.FocusItem |> dispatch) List.empty
                ]
            ]
            Html.divc "column is-one-fifth iconColumn" [
                tryIcon (App.Init.tryFindIcon item.Icon |> Option.defaultWith (fun () -> App.Init.IconSearchType.FAIcon item.Icon))
            ]
            Html.divc "column" [
                Html.label [
                    text item.Name
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty stripped)]
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty item.Acls)]
                ]
            ]
        ]

    let renderRootView title items dispatch =
        Html.div [
            Html.label [
                text title
            ]
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

    let renderRemote title rdState reqMsg okRenderer (dispatch:Dispatch<Msg>) =
        let buttonText = text title
        match rdState with
        | RemoteData.NotRequested ->
            Html.button [
                buttonText
                onClick(fun e -> dispatch reqMsg) []
            ]
        | RemoteData.InFlight ->
            Html.button [
                Attr.disabled true
                buttonText
            ]
        | RemoteData.Responded(Ok(data)) ->
            okRenderer data
        | RemoteData.Responded(Error exn) ->
            Html.divc "error" [
                text (Core.pretty exn)
            ]

let css = [

    rule "label>span.info" Gen.CssRules.titleIndicator  
    rule "div.iconColumn" [
        Css.height (em 1.0)
        Css.width (em 1.0)
        Css.flexShrink 0
    ]
    rule "div.buttonColumn" [
        Css.height (em 1.0)
        Css.width (em 2.5)
        Css.flexShrink 0
    ]
    rule ".tile .field" [
        Css.marginRight (px 5)
    ]
    rule ".tile .field .control .box" [
        Css.minWidth (px 450)
    ]
]

let view appMode =
    let store, dispatch = appMode |> Store.makeElmish init update ignore

    // let selected : IStore<NavRootResponse option> = Store.make( None )
    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store ]
        data_ "file" "Root"
        Bind.el(store |> Store.map MLens.getRootTab, fun rt ->
            let rootTab =
                {
                    Name= "Root"
                    TabClickMsg= Msg.TabChange RootTabs.RootMain
                    IsActive= rt = RootTabs.RootMain
                    Render=
                        let r data = Renderers.renderRootView "" data dispatch
                        fun () ->
                            Bind.el(store |> Store.map MLens.getNavRootState, fun nrs ->
                                Renderers.renderRemote "Root" nrs (RemoteMsg.Request () |> Msg.NavRootMsg) r dispatch
                            )
                }

            let pathTab =
                {
                    Name="Path"
                    TabClickMsg= Msg.TabChange RootTabs.RootSub
                    IsActive= rt = RootTabs.RootSub
                    Render=
                        fun () ->
                            Html.div [
                                // display the path input properly based on current state
                                Bind.el(store |> Store.map MLens.getNavPathState,
                                    Renderers.renderPathInput store dispatch
                                )
                                Bind.el(store |> Store.map MLens.getNavPathState,
                                    (fun nps ->
                                        let r (x:NavPathResponse) = Renderers.renderRootView $"Sub:{x.Path}" x.Items dispatch
                                        Renderers.renderRemote $"Path:{store.Value.Path}" nps (RemoteMsg.Request () |> Msg.NavPathMsg) r dispatch
                                    )
                                )
                            ]
                }

            // bulma tabs
            tabs [
                rootTab
                pathTab
                {
                    Name="Edit"
                    TabClickMsg= Msg.TabChange RootTabs.RootEditor
                    IsActive= rt = RootTabs.RootEditor
                    Render=
                        let gfi= store |> Store.map MLens.getFocusedItem
                        let gAcl= store |> Store.map MLens.getAclTypes
                        let r : _ -> SutilElement =
                                function
                                | _, None
                                | None, _ -> Html.div []
                                | Some item, Some aclTypes ->
                                    let r = NavEditor.renderEditor aclTypes (item, store |> Store.map MLens.getFocusedItem) (Msg.EditorMsg >> dispatch)
                                    r
                        fun () ->
                            Bind.el2 gfi gAcl r

                }
            ] dispatch
        )

    ]
    |> withStyle css