
module App.Components.Root

open Sutil
open Sutil.CoreElements

open App.Adapters.Config
open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen
open App.Components.Gen.Icons

let console = Fable.Core.JS.console

type Model = {
    AppMode: ConfigType<string>
    NavRootState : RemoteData<NavRootResponse[]>
    NavPathState : RemoteData<string*NavRootResponse[]>
    FocusedItem : NavRootResponse option
    // this should not be able to change while a request is in flight
    Path: string
}

let getNavRootState x = x.NavRootState
let getFocusedItem x = x.FocusedItem
let getNavPathState x = x.NavPathState
let getPath x = x.Path

type Msg =
    | NavRootMsg of RemoteMsg<unit, NavRootResponse[]>
    | NavPathMsg of RemoteMsg<unit, NavRootResponse[]>

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
    {AppMode = appMode; NavRootState = NotRequested; NavPathState = NotRequested; FocusedItem = None; Path = ""}, cmd

let update msg model : Model * Cmd<Msg> =
    match msg,model with
    // block actions
    | NavRootMsg (Request _), {NavRootState= InFlight} -> model, Cmd.none // no spamming requests
    | NavPathMsg (Request _), {NavPathState= InFlight} -> model, Cmd.none

    // actions
    | NavRootMsg (Request x), _ ->
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
            {model with NavPathState= InFlight}, Commands.getNavPath (accessToken, model.Path)
    | NavPathMsg (Response x), _ ->
        let v = x |> Result.map(fun items -> model.Path, items)
        {model with NavPathState= Responded v}, Cmd.none


module Renders =

    let renderLabeledField name value =
        Html.div [ Html.label [text name]; Html.pre [text value]]

    let renderRootView items =
        Html.div [
            // https://fontawesome.com/docs/web/setup/get-started
            Html.ul [
                for item in items do
                    Html.li [
                        data_ "rootItem.Icon" item.Icon
                        data_ "rootItem" (Core.serialize item)
                        for (name,v) in
                            [
                                "Id", string item.Id
                                "Name", string item.Name
                                "Path", string item.Path
                                // "Parent", string item.Parent
                                "Type", string item.Type
                                "Description", string item.Description
                                "Icon", string item.Icon
                                "Weight", string item.Weight
                            ] do
                            // assuming they are all mui
                            tryIcon (App.Init.tryFindIcon item.Icon |> Option.defaultWith (fun () -> App.Init.IconSearchType.FAIcon item.Icon))

                            // Bulma.FontAwesome.fa item.Icon
                            renderLabeledField name v
                        for acl in item.Acls do
                            Html.ul [
                                Html.li [Html.pre [text (Core.pretty acl)]]
                            ]
                    ]
            ]
        ]

    let renderEditor (value:NavRootResponse) (dispatch: Dispatch<Msg>) = 
        Html.div [
            text (Core.pretty value)
        ]

let view appMode =
    let model, dispatch = appMode |> Store.makeElmish init update ignore

    // let selected : IStore<NavRootResponse option> = Store.make( None )
    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]
        data_ "file" "Root"
        Html.div [
            Html.label [
                text "Path:"
            ]
            // display the path input properly based on current state
            Bind.el(model |> Store.map getNavPathState,
                function
                | InFlight ->
                    Html.input [
                        type' "text"
                        // Attr.value path
                        Bind.attr ("value", model |> Store.map getPath)
                    ]

                | Responded (Error _)
                | NotRequested ->
                    Html.input [
                        type' "text"
                        autofocus
                        Bind.attr ("value", model |> Store.map getPath)
                    ]

                | Responded(Ok(path,_ )) ->
                    Html.input [
                        type' "text"
                        // Attr.value path
                        Bind.attr ("value", model |> Store.map getPath)
                    ]
            )
        ]
        Bind.el(model |> Store.map getNavRootState, fun mis ->
            let buttonText = text "Nav Root"
            match mis with
            | RemoteData.NotRequested ->
                Html.button [
                    buttonText
                    onClick(fun e -> dispatch (Msg.NavRootMsg (RemoteMsg.Request ()))) []
                ]
            | RemoteData.InFlight ->
                Html.button [
                    Attr.disabled true
                    buttonText
                ]
            | RemoteData.Responded(Ok(data)) ->
                Bind.el(model |> Store.map getFocusedItem, 
                    function
                    | None -> Renders.renderRootView data
                    | Some v ->
                        Renders.renderEditor v dispatch
                )
            | RemoteData.Responded(Error exn) ->
                Html.divc "error" [
                    text (Core.pretty exn)
                ]
        )
    ]