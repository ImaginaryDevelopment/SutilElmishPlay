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

open App.Components.Gen
open App.Components.Gen.Icons

type RootTabs =
    | RootMain
    | RootSub
    | RootEditor

type Model = {
    AppMode: ConfigType<string>
    NavRootState : RemoteData<NavRootResponse[]>
    NavPathState : RemoteData<string*NavRootResponse[]>
    FocusedItem : NavRootResponse option
    RootTab: RootTabs
    // this should not be able to change while a request is in flight
    Path: string
}

module MLens =
    let getNavRootState x = x.NavRootState
    let getFocusedItem x = x.FocusedItem
    let getNavPathState x = x.NavPathState
    let getPath x = x.Path
    let getRootTab x = x.RootTab

type EditorMsgType =
    | ChangeFocus of NavRootResponse option
    | EditProp of prop:string * nextValue :string

type Msg =
    | NavRootMsg of RemoteMsg<unit, NavRootResponse[]>
    | NavPathMsg of RemoteMsg<unit, NavRootResponse[]>
    | TabChange of RootTabs
    | EditorMsg of EditorMsgType
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

let init appMode =
    let cmd: Cmd<Msg> = 
        match appMode with
        | Demo -> Cmd.none
        | Auth token -> Commands.getNavRoot token
    {AppMode = appMode; NavRootState = NotRequested; NavPathState = NotRequested; FocusedItem = None; RootTab= RootTabs.RootMain; Path = ""}, cmd

let update msg (model:Model) : Model * Cmd<Msg> =
    printfn "Root Update running: %A ('%s')" msg model.Path
    let block title =
        printfn "Blocked: %s (%A)" title msg
        model, Cmd.none
    match msg, model with
    // block actions
    | NavRootMsg (Request _), {NavRootState= InFlight} -> block "InFlight NavRootMsg"
    | NavPathMsg (Request _), {NavPathState= InFlight} -> model, Cmd.none
    | PathReq, {NavPathState= InFlight} -> block "PathReq InFlight"
    | PathReq, {NavPathState= _; Path= NonValueString _} -> block "PathReq NoPath"
    | PathChange _, {NavPathState=InFlight} -> model, Cmd.none
    | TabChange v, {RootTab= y} when v = y -> model, Cmd.none

    // actions
    | TabChange v, _ -> {model with RootTab= v}, Cmd.none
    | EditorMsg(EditorMsgType.ChangeFocus None), _ -> {model with FocusedItem = None}, Cmd.none
    | EditorMsg(EditorMsgType.ChangeFocus (Some item)), _ -> {model with FocusedItem = Some (clone<NavRootResponse> item)}, Cmd.none
    | EditorMsg(EditorMsgType.EditProp(name,value)), _ -> model, Cmd.none
    | PathChange next, _ ->
        printfn "PathChange '%s' to '%s'" model.Path next
        {model with Path= next}, Cmd.none
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
    | NavRootMsg (Response x ), _ -> {model with NavRootState= Responded x}, Cmd.none

    | NavPathMsg (Request ()), _ ->
        match model.AppMode with
        | ConfigType.Demo ->
            {model with NavPathState= Responded(Ok (model.Path,dummyData))}, Cmd.none
        | ConfigType.Auth accessToken ->
            printfn "Requesting Path '%s'" model.Path
            {model with NavPathState= InFlight}, Commands.getNavPath (accessToken, model.Path)
    | NavPathMsg (Response x), _ ->
        let v = x |> Result.map(fun items -> model.Path, items)
        {model with NavPathState= Responded v}, Cmd.none

module Renders =

    let renderLabeledField name value =
        Html.div [ Html.label [text name]; Html.pre [text value]]

    let renderItemView (item:NavRootResponse) (dispatch: Dispatch<Msg>) =  
        let stripped = cloneExcept(item, ["Acls"])
        Html.divc "columns" [
            Html.divc "column is-one-fifth buttonColumn" [
                Html.button [
                    tryIcon (App.Init.IconSearchType.MuiIcon "Edit")
                    onClick (fun _ -> item |> Some |> ChangeFocus |> Msg.EditorMsg  |> dispatch) List.empty
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

    let renderIconEditor (name: string) (dispatch: Dispatch<Msg>) =
        Html.divc "box" [
            tryIcon (App.Init.IconSearchType.MuiIcon name)
            formField [ text "Icon Name"] [
                Html.inputc "input" [
                    type' "text"
                    Attr.value name
                    Handlers.onValueChange dispatch (fun v -> EditProp("Name",v) |> Msg.EditorMsg)
                ]
            ]

        ]

    let renderPathInput (model: IStore<Model>) dispatch =
                function
                | InFlight ->
                    Html.input [
                        type' "text"
                        Attr.value model.Value.Path
                        Attr.disabled true
                        // Bind.attr ("value", model |> Store.map getPath)
                    ]

                | Responded _
                | NotRequested  ->
                    Html.div [

                        Html.input [
                            type' "text"
                            autofocus
                            Bind.attr ("value", model |> Store.map MLens.getPath)
                            Handlers.onValueChange dispatch PathChange
                            // on "change" (Handlers.getValue >> PathChange >> dispatch) List.empty
                        ]
                        Html.button [
                            text "Fetch"
                            onClick(fun _ -> printfn "Dispatching pathReq"; dispatch Msg.PathReq) List.empty
                        ]
                    ]

    // renames will go a different route, no path editing
    let renderEditor (value:NavRootResponse) (dispatch: Dispatch<Msg>) = 
        Html.divc "panel" [
            // path
            Html.pc "panel-heading" [ text $"{value.Name}: {value.Path}"]
            // columns2 [ Html.labelc "label" [ text "Path" ]] [Html.divc "control" [ text ]]
            Html.divc "panel-block" [
                // formField [ text "hello"] []
                formField [text "Icon"] [
                    renderIconEditor value.Icon dispatch
                ]
                Html.buttonc "button" [
                    text "Cancel"
                    onClick (fun _ -> EditorMsgType.ChangeFocus None |> Msg.EditorMsg |> dispatch) []
                ]
            ]
            text (Core.pretty value)
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

    rule "label>span.info" [ // https://www.scientificpsychic.com/etc/css-mouseover.html
        Css.custom("border-bottom","thin dotted")
        Css.backgroundColor "#ffeedd"
        // Css.paddingLeft 5
        Css.marginLeft 5
    ]
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
                        let r data = Renders.renderRootView "" data dispatch
                        fun () ->
                            Bind.el(store |> Store.map MLens.getNavRootState, fun nrs ->
                                Renders.renderRemote "Root" nrs (RemoteMsg.Request () |> Msg.NavRootMsg) r dispatch
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
                                    Renders.renderPathInput store dispatch
                                )
                                Bind.el(store |> Store.map MLens.getNavPathState,
                                    (fun nps ->
                                        let r (path,data)= Renders.renderRootView $"Sub:{path}" data dispatch
                                        Renders.renderRemote $"Path:{store.Value.Path}" nps (RemoteMsg.Request () |> Msg.NavPathMsg) r dispatch
                                    )
                                )
                            ]
                }
            tabs [
                rootTab
                pathTab
                {
                    Name="Edit"
                    TabClickMsg= Msg.TabChange RootTabs.RootEditor
                    IsActive= rt = RootTabs.RootEditor
                    Render=
                        fun () ->
                            Bind.el(store |> Store.map MLens.getFocusedItem, 
                                function
                                | None -> Html.div []
                                | Some item -> Renders.renderEditor item dispatch
                            )

                }
            ] dispatch
        )

        // Bind.el(store |> Store.map MLens.getNavRootState, fun mis ->
        //     let buttonText = text "Nav Root"
        //     match mis with
        //     | RemoteData.NotRequested ->
        //         Html.button [
        //             buttonText
        //             onClick(fun e -> dispatch (Msg.NavRootMsg (RemoteMsg.Request ()))) []
        //         ]
        //     | RemoteData.InFlight ->
        //         Html.button [
        //             Attr.disabled true
        //             buttonText
        //         ]
        //     | RemoteData.Responded(Ok(data)) ->
        //         Bind.el(store |> Store.map MLens.getFocusedItem, 
        //             function
        //             | None -> Renders.renderRootView data dispatch
        //             | Some v ->
        //                 Renders.renderEditor v dispatch
        //         )
        //     | RemoteData.Responded(Error exn) ->
        //         Html.divc "error" [
        //             text (Core.pretty exn)
        //         ]
        // )
    ]
    |> withStyle css