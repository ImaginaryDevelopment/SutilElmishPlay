
module App.Components.Root

open Sutil
open Sutil.CoreElements

open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen

type Model = {
    AccessToken: string
    NavRootState : RemoteData<NavRootResponse[]>
    FocusedItem : NavRootResponse option
}

let getNavRootState x = x.NavRootState
let getFocusedItem x = x.FocusedItem
type Msg =
    | NavRootMsg of RemoteMsg<unit, NavRootResponse[]>

module Commands =
    let getNavRoot token =
        let a () =
            async {
                let! resp = App.Adapters.Api.getNavRoot token
                let resp2 = Response resp
                return Msg.NavRootMsg resp2
            }
        Cmd.OfAsync.perform a () id

let init token =
    let cmd: Cmd<Msg> = Commands.getNavRoot token
    {AccessToken= token; NavRootState= InFlight; FocusedItem= None}, cmd

let update msg model : Model * Cmd<Msg> =
    match msg,model with
    | NavRootMsg (Request _), {NavRootState= InFlight} -> model, Cmd.none // no spamming requests
    | NavRootMsg (Request _), _ -> {model with NavRootState= InFlight}, Commands.getNavRoot model.AccessToken
    | NavRootMsg (Response x ), _ -> {model with NavRootState= Responded x}, Cmd.none

module Renders =

    let renderLabeledField name value =
        Html.div [ Html.label [text name]; Html.pre [text value]]
    let renderRootView items =
        Html.div [
            // https://fontawesome.com/docs/web/setup/get-started
            Html.parse """<i class="fa-solid fa-user"></i>"""
            Html.parse """<p><i class="fa fa-solid fa-lastfm"></i></p>"""
            Html.ul [
                for item in items do
                    Html.li [
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
                            Bulma.FontAwesome.fa item.Icon
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

let view token =
    let model, dispatch = token |> Store.makeElmish init update ignore

    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]
        data_ "file" "Root"
        Html.div [
            text "Hello root"
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