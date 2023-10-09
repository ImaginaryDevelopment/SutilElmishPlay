
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
    FocusedItem : NavRootResponse option
}

let getNavRootState x = x.NavRootState
type Msg =
    | NavRootMsg of RemoteMsg<unit, NavRootResponse[]>

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

let init appMode =
    {AppMode = appMode; NavRootState = NotRequested; FocusedItem = None}, Cmd.none

module Commands =
    let getNavRoot token =
        async {
            let! resp = App.Adapters.Api.getNavRoot token
            let resp2 = Response resp
            return Msg.NavRootMsg resp2
        }

let update msg model : Model * Cmd<Msg> =
    match msg,model with
    | NavRootMsg (Request _), {NavRootState= InFlight} -> model, Cmd.none // no spamming requests
    | NavRootMsg (Request x), _ ->
        match model.AppMode with
        | ConfigType.Demo ->
            {model with NavRootState=Responded (Ok dummyData)}, Cmd.none
        | ConfigType.Auth accessToken ->
            {model with NavRootState= InFlight}, Cmd.OfAsync.perform Commands.getNavRoot accessToken id
    | NavRootMsg (Response x ), _ -> {model with NavRootState= Responded x}, Cmd.none


let view appMode =
    let model, dispatch = appMode |> Store.makeElmish init update ignore
    let selected : IStore<NavRootResponse option> = Store.make( None )
    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]
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
            | RemoteData.Responded(Ok data) ->
                Html.div [
                    Html.pre [
                        // prop.custom("data-status","200")
                        data_ "status" "200"
                        text (Core.pretty data)
                    ]
                    Html.div[
                        text "Icon?"
                    ]
                ]
            | RemoteData.Responded(Error exn) ->
                Html.divc "error" [
                    text (Core.pretty exn)
                ]
        )
    ]