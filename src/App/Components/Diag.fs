module App.Components.Diag

open Sutil
open Sutil.CoreElements

type ErrorType = exn
type RemoteData<'t> =
    | NotRequested
    | InFlight
    | Responded of Result<'t,ErrorType>

type RemoteMsg<'tReq, 'tResp> =
    | Request of 'tReq
    | Response of Result<'tResp,ErrorType>

type Model = {
    AccessToken: string
    MyInfoState: RemoteData<obj>
}

let getMyInfoState x = x.MyInfoState
type Msg =
    | MyInfo of RemoteMsg<unit, obj>

let init token () =
    {AccessToken= token; MyInfoState = NotRequested}, Cmd.none

module Commands =
    let getMyInfo token =
        async {
            let! resp = App.Adapters.Api.getMyInfo token
            match resp with
            | Choice1Of2 value -> return Msg.MyInfo (Response(Ok value))
            | Choice2Of2 err -> return Msg.MyInfo (Response(Error err))
        }

let update msg model : Model * Cmd<Msg> =
    match msg,model with
    | MyInfo (Request _), {MyInfoState= InFlight} -> model, Cmd.none // no spamming requests
    | MyInfo (Request x), _ -> {model with MyInfoState = InFlight}, Cmd.OfAsync.perform Commands.getMyInfo model.AccessToken id

let view token =
    let (model:IStore<Model>, dispatch) = () |> Store.makeElmish (init token) update ignore

    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]
        Html.div [
            text "Hello diag"
        ]
        Bind.el(model |> Store.map getMyInfoState, fun mis ->
            let buttonText = text "My Info"
            match mis with
            | RemoteData.NotRequested ->
                Html.button [
                    buttonText
                    onClick(fun e -> dispatch (Msg.MyInfo(RemoteMsg.Request ()))) []
                ]
            | RemoteData.InFlight ->
                Html.button [
                    Attr.disabled true
                    buttonText
                ]
            | RemoteData.Responded(Ok(data)) ->
                Html.div [
                    text "yay data?"
                ]
        )
    ]