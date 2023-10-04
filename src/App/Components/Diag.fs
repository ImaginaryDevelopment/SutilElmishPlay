module App.Components.Diag

open Sutil
open Sutil.CoreElements

open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen


type RemoteStates = {
    MyInfoState: RemoteData<MyInfoResponse>
    NavRootState: RemoteData<NavRootResponse[]>
    AclState: RemoteData<Acl[]>
}

type Model = {
    AccessToken: string
    States: RemoteStates
}

type Msg =
    | MyInfo of RemoteMsg<unit, MyInfoResponse>
    | NavRoot of RemoteMsg<unit,NavRootResponse[]>
    | Acl of RemoteMsg<unit,Acl[]>

// enable mass scaffolding of api endpoint testers
type GenericFetchArgs<'t> = {
    Title: string
    GetObserver: IStore<Model> -> (Model -> RemoteData<'t>) -> System.IObservable<RemoteData<'t>>
    GetState: Model -> RemoteData<'t>
    SetState: Model -> RemoteData<'t> -> Model
    Fetch: string -> Async<Result<'t,ErrorType>>
    WrapMsg: RemoteMsg<unit,'t> -> Msg
    // assumes the only arguments are access token
    // Command: string -> Async<'t>
}

module GenericFetcher =

    // let createModule<'t> (fObs: _ -> _) (fState: Model -> RemoteData<'t>, fSetState: Model -> RemoteData<'t> -> Model) (fFetch: string -> Async<Result<'t,_>>) (fMsgWrap: RemoteMsg<_,'t> -> Msg) fCmd =
    let createModule<'t> gfa =
        let getCmd token =
            async{
                let! resp = gfa.Fetch token
                return gfa.WrapMsg(Response resp)
            }
        let update msg (model: Model) =
            match msg, gfa.GetState model with
            | Request _, InFlight -> model, Cmd.none
            | Request _, _ ->  gfa.SetState model InFlight, Cmd.OfAsync.perform getCmd model.AccessToken id
            | Response x, _ -> gfa.SetState model (Responded x), Cmd.none

        let view (store, dispatch) =
            let inline dispatchReqMsg _ = RemoteMsg.Request () |> gfa.WrapMsg |> dispatch

            Bind.el(gfa.GetObserver store gfa.GetState, fun (mis:RemoteData<'t>) ->
                let buttonText = text gfa.Title
                match mis with
                | RemoteData.NotRequested ->
                    Html.button [
                        buttonText
                        onClick dispatchReqMsg []
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
        update, view

let getMyInfoState x = x.States.MyInfoState

let init token () =
    {AccessToken=token;States={MyInfoState=NotRequested;NavRootState=NotRequested;AclState=NotRequested}}, Cmd.none

module Commands =
    let getMyInfo token =
        async {
            let! resp = App.Adapters.Api.getMyInfo token
            return Msg.MyInfo(Response resp)
        }
    // let getNavRoot token =
    //     async {
    //         let! resp = App.Adapters.Api.getNavRoot token
    //         return Msg.NavRoot(Response resp)
    //     }

let setAState f model = {model with States=f model.States}

let navRootArr =
    GenericFetcher.createModule<NavRootResponse[]> {
        Title= "NavRootResponse[]"
        GetObserver= fun store f -> store |> Store.map (fun (model: Model) -> model.States.NavRootState)
        GetState= fun model -> model.States.NavRootState
        SetState= fun model next -> setAState (fun states -> {states with NavRootState = next}) model
        Fetch= App.Adapters.Api.getNavRoot
        WrapMsg= Msg.NavRoot
    }
()

let updateAcl, viewAcls =
    let gma = {
            GetState= fun model -> model.States.AclState
            WrapMsg= Msg.Acl
        }
    let update  =
        Gen.GenericFetcher.createUpdate {
            Gma= gma
            SetState= fun (model:Model) next -> model |> setAState (fun states -> {states with AclState= next})// 'tModel -> RemoteData<'t> -> 'tModel
            GetArgs= fun model -> model.AccessToken
            Fetch= App.Adapters.Api.getAcls // 'tFetchArg -> Async<Result<'t,ErrorType>>
        }
    let view =
        Gen.GenericFetcher.createView
            {
            Title= "Acls"
            Gma= {
                GetState= fun model -> model.States.AclState
                WrapMsg= Msg.Acl
            }
            GetObserver= fun store f -> store |> Store.map f
            GetReqArg= fun model -> ()
            }
    update,view

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg, model with
    | MyInfo (Request _), {States={MyInfoState= InFlight}} -> model, Cmd.none // no spamming requests
    | MyInfo (Request _), _ -> model |> setAState (fun states -> {states with MyInfoState = InFlight}), Cmd.OfAsync.perform Commands.getMyInfo model.AccessToken id
    | MyInfo (Response x ), _ -> model |> setAState (fun states -> {states with MyInfoState = Responded x}), Cmd.none

    | NavRoot rr, model ->
        let u,_ = navRootArr
        u rr model
    | Acl rr, model ->
        updateAcl rr model
()

let view token =
    let (store:IStore<Model>, dispatch) = () |> Store.makeElmish (init token) update ignore
    let tabStore =
        let tabParent =
            let myInfoComponent =
                Gen.GenericFetcher.createView {
                            Title= "MyInfo"
                            Gma= {
                                GetState= fun model -> model.States.MyInfoState
                                WrapMsg= Msg.MyInfo
                            }
                            GetObserver= fun store f -> store |> Store.map f
                            GetReqArg= fun _ -> ()
                        } (store,dispatch)
            {
                ActiveTab= 0
                Tabs = [|
                    {
                        Label="MyInfo"
                        Value=1
                        Component= myInfoComponent
                    }
                    {
                        Label="Root"
                        Value=1
                        Component= App.Components.Root.view token
                    }
                    {
                        Label= "Acls"
                        Value= 1
                        Component = Gen.GenericFetcher.createView {
                            Title= "Acls"
                            Gma= {
                                GetState= fun model -> model.States.AclState
                                WrapMsg= Msg.Acl
                            }
                            GetObserver= fun store f -> store |> Store.map f
                            GetReqArg= fun _ -> ()
                        } (store,dispatch)
                    }

                |]
            }
        () |> Store.makeElmishSimple (fun _ -> tabParent) Tabs.update ignore

    let _,v = navRootArr


    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store ]
        Html.div [
            text "Hello diag"
        ]
        App.Components.Gen.Tabs.view (Choice2Of2 tabStore)
        Bind.el(store |> Store.map getMyInfoState, fun mis ->
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
        v(store,dispatch)
        viewAcls(store,dispatch)
    ]