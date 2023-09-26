module App.Components.Diag

open Sutil
open Sutil.CoreElements

open App.Adapters.Html
open App.Adapters.Api

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
    MyInfoState: RemoteData<MyInfoResponse>
}

let getMyInfoState x = x.MyInfoState
type Msg =
    | MyInfo of RemoteMsg<unit, MyInfoResponse>

let init token () =
    {AccessToken= token; MyInfoState = NotRequested}, Cmd.none

module Commands =
    let getMyInfo token =
        async {
            let! resp = App.Adapters.Api.getMyInfo token
            return Msg.MyInfo(Response resp)
        }

let update msg model : Model * Cmd<Msg> =
    match msg,model with
    | MyInfo (Request _), {MyInfoState= InFlight} -> model, Cmd.none // no spamming requests
    | MyInfo (Request x), _ -> {model with MyInfoState = InFlight}, Cmd.OfAsync.perform Commands.getMyInfo model.AccessToken id
    | MyInfo (Response x ), _ -> {model with MyInfoState = Responded x}, Cmd.none

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
                    Html.pre [
                        // prop.custom("data-status","200")
                        data_ "status" "200"
                        text (Core.pretty data)
                    ]
                ]
            | RemoteData.Responded(Error exn) ->
                Html.divc "error" [
                    text (Core.pretty exn)
                ]
        )
    ]