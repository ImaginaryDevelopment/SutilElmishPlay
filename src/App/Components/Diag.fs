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

let getMyInfoState x = x.States.MyInfoState
type DiagInitArgs= {
    Token: string
}

let init dia =
    {AccessToken=dia.Token;States={MyInfoState=NotRequested;NavRootState=NotRequested;AclState=NotRequested}}, Cmd.none

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

let updateNavRoot, viewNavRoot =
    let gma = {
            GetState= fun model -> model.States.NavRootState
            WrapMsg= Msg.NavRoot
        }
    let update  =
        Gen.GenericFetcher.createUpdate {
            Gma= gma
            SetState= fun (model:Model) next -> model |> setAState (fun states -> {states with NavRootState= next})// 'tModel -> RemoteData<'t> -> 'tModel
            GetArgs= fun model -> model.AccessToken
            Fetch= App.Adapters.Api.getNavRoot
        }
    let view =
        Gen.GenericFetcher.createView {
            Title= "DiagNavRoot"
            Gma= gma
            GetObserver= fun store f -> store |> Store.map f
            GetReqArg= fun model -> ()
        }
    update,view


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
            Gma= gma
            GetObserver= fun store f -> store |> Store.map f
            GetReqArg= fun model -> ()
            }
    update,view

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    printfn "Diag update"
    match msg, model with
    | MyInfo (Request _), {States={MyInfoState= InFlight}} -> model, Cmd.none // no spamming requests
    | MyInfo (Request _), _ -> model |> setAState (fun states -> {states with MyInfoState = InFlight}), Cmd.OfAsync.perform Commands.getMyInfo model.AccessToken id
    | MyInfo (Response x ), _ -> model |> setAState (fun states -> {states with MyInfoState = Responded x}), Cmd.none

    | NavRoot rr, model ->
        updateNavRoot rr model
    | Acl rr, model ->
        updateAcl rr model
()

let view dia =
    let (store:IStore<Model>, dispatch) = dia |> Store.makeElmish init update ignore
    let di = store.Subscribe(fun v -> Browser.Dom.window.localStorage.setItem("diagModel", Fable.Core.JS.JSON.stringify(v)))

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
                Name="DiagTabRoot"
                ActiveTab= 0
                Tabs = [|
                    {
                        Label= "MyInfo"
                        Value= 0
                        Component= myInfoComponent
                    }
                    {
                        Label= "Root"
                        Value= 1
                        Component= viewNavRoot (store,dispatch)
                    }
                    {
                        Label= "Acls"
                        Value= 2
                        Component = viewAcls (store,dispatch)
                    }
                |]
            }
        () |> Store.makeElmishSimple (fun _ -> tabParent) Tabs.update ignore

    // let _,v = navRootArr


    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store; di ]
        App.Components.Gen.Tabs.view (Choice2Of2 tabStore)
    ]