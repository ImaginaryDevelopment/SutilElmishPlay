module App.Components.Diag

open Sutil
open Sutil.CoreElements

open BReusable

open App.Adapters.Config
open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen
open App.Components.Gen.Icons


type RemoteStates = {
    MyInfoState: RemoteData<MyInfoResponse>
    NavRootState: RemoteData<NavItem[]>
    AclState: RemoteData<Acl[]>
    AclParamSearchState: RemoteData<AclSearchResponse>
}

type Model = {
    AppMode: ConfigType<string>
    States: RemoteStates
    AclParamSearchInput: AclRefValueArgs
}

[<RequireQualifiedAccess>]
module private MLens =
    let getAppMode x = x.AppMode
    let getStates x = x.States

    let setAclParamSearchState v x = {
        x with
            States = {
                x.States with
                    AclParamSearchState = v
            }
    }


type Msg =
    | MyInfo of RemoteMsg<unit, MyInfoResponse>
    | NavRoot of RemoteMsg<unit, NavItem[]>
    | Acl of RemoteMsg<unit, Acl[]>
    | AclParam of RemoteMsg<AclRefValueArgs, AclSearchResponse>

// enable mass scaffolding of api endpoint testers

let getMyInfoState x = x.States.MyInfoState

type DiagInitArgs = { AppMode: ConfigType<string> }

let init (dia: DiagInitArgs) =
    {
        AppMode = dia.AppMode
        States = {
            MyInfoState = NotRequested
            NavRootState = NotRequested
            AclState = NotRequested
            AclParamSearchState = NotRequested
        }
        AclParamSearchInput = { AclName = ""; SearchText = "" }
    },
    Cmd.none

module Commands =
    let getMyInfo token =
        let f () =
            async {
                let! resp = App.Adapters.Api.getMyInfo token
                return Msg.MyInfo(Response resp)
            }

        Cmd.OfAsync.perform f () id

// let getAclParamSearch token req : Cmd<Msg> =
//     let f () = async {

//         let! resp = App.Adapters.Api.searchAclRefValues token req
//         return Msg.AclParam (Response resp)
//     }

//     Cmd.OfAsync.perform f () id


let setAState f model = { model with States = f model.States }

let updateNavRoot, viewNavRoot =
    let gma = {
        GetState = fun model -> model.States.NavRootState
        WrapMsg = Msg.NavRoot
    }

    let update =
        Gen.GenericFetcher.createUpdate {
            Gma = gma
            SetState = fun (model: Model) next -> model |> setAState (fun states -> { states with NavRootState = next }) // 'tModel -> RemoteData<'t> -> 'tModel
            GetArgs = fun model -> model.AppMode
            Fetch =
                function
                | ConfigType.Auth token -> App.Adapters.Api.NavItems.getNavRoot token ()
                | ConfigType.Demo -> Async.ofResult (Ok Root.dummyData)
        }

    let view =
        Gen.GenericFetcher.createView {
            Title = "DiagNavRoot"
            Gma = gma
            GetObserver = fun store f -> store |> Store.map f
            GetReqArg = fun model -> ()
        }

    update, view


let updateAcl, viewAcls =
    let gma = {
        GetState = fun model -> model.States.AclState
        WrapMsg = Msg.Acl
    }

    let update =
        Gen.GenericFetcher.createUpdate {
            Gma = gma
            SetState = fun (model: Model) next -> model |> setAState (fun states -> { states with AclState = next }) // 'tModel -> RemoteData<'t> -> 'tModel
            GetArgs = fun model -> model.AppMode
            Fetch =
                function
                | Demo -> Async.ofResult (Ok Array.empty)
                | Auth token -> App.Adapters.Api.getAcls token () // 'tFetchArg -> Async<Result<'t,ErrorType>>
        }

    let view =
        Gen.GenericFetcher.createView {
            Title = "Acls"
            Gma = gma
            GetObserver = fun store f -> store |> Store.map f
            GetReqArg = fun model -> ()
        }

    update, view

let updateAclSearch, viewAclSearch =
    let gma = {
        GetState = MLens.getStates >> fun m -> m.AclParamSearchState
        WrapMsg = Msg.AclParam
    }

    let update =
        Gen.GenericFetcher.createUpdate {
            Gma = gma
            SetState =
                fun (model: Model) next ->
                    model
                    |> setAState (fun states -> {
                        states with
                            AclParamSearchState = next
                    }) // 'tModel -> RemoteData<'t> -> 'tModel
            GetArgs = fun model -> (model.AppMode, model.AclParamSearchInput)
            Fetch =
                function
                // TODO: validate search params?
                | Auth token, si -> App.Adapters.Api.searchAclRefValues token si
                | Demo, _ -> Async.ofResult (Error <| System.Exception("Not Implemented"))
        }

    let view =
        Gen.GenericFetcher.createView {
            Title = "Acl Search"
            Gma = gma
            GetObserver = fun store f -> store |> Store.map f
            GetReqArg = fun model -> model.AclParamSearchInput
        }

    update, view

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    printfn "Diag update"

    match msg, model with
    | MyInfo(Request _), { States = { MyInfoState = InFlight } } -> model, Cmd.none // no spamming requests
    | MyInfo(Request _), _ ->
        match model.AppMode with
        | Auth token ->
            model |> setAState (fun states -> { states with MyInfoState = InFlight }), Commands.getMyInfo token
        | Demo ->
            model
            |> setAState (fun states -> {
                states with
                    MyInfoState = Responded(Error(System.Exception("Not Implemented")))
            }),
            Cmd.none
    | MyInfo(Response x), _ ->
        model
        |> setAState (fun states -> {
            states with
                MyInfoState = Responded x
        }),
        Cmd.none

    | NavRoot rr, _ -> updateNavRoot rr model
    | Acl rr, _ -> updateAcl rr model
    | AclParam rr, _ -> updateAclSearch rr model

// | AclParam(Request _), {States = {AclParamSearchState = InFlight}} -> model, Cmd.none
// | AclParam(Request req), _ ->
//     match model.AppMode with
//     | Auth token ->
//         model |> MLens.setAclParamSearchState InFlight, Commands.searchAclRefValues token
//     | Demo ->
//         model
//         |> setAState (fun states -> {
//             states with
//                 AclParamSearchState = Responded(Error(System.Exception("Not Implemented")))
//         }),
//         Cmd.none

()

let view dia =
    let (store: IStore<Model>, dispatch) = dia |> Store.makeElmish init update ignore

    Core.toGlobalWindow "diag_model" store.Value

    let tabStore =
        let tabParent =
            let myInfoComponent =
                Gen.GenericFetcher.createView
                    {
                        Title = "MyInfo"
                        Gma = {
                            GetState = fun model -> model.States.MyInfoState
                            WrapMsg = Msg.MyInfo
                        }
                        GetObserver = fun store f -> store |> Store.map f
                        GetReqArg = fun _ -> ()
                    }
                    (store, dispatch)

            {
                Name = "DiagTabRoot"
                ActiveTab = 0
                Tabs = [|
                    {
                        Label = "MyInfo"
                        Value = 0
                        Component = myInfoComponent
                    }
                    {
                        Label = "Root"
                        Value = 1
                        Component = viewNavRoot (store, dispatch)
                    }
                    {
                        Label = "Acls"
                        Value = 2
                        Component = viewAcls (store, dispatch)
                    }
                    {
                        Label = "AclSearch"
                        Value = 3
                        Component = viewAclSearch (store, dispatch)
                    }
                |]
            }

        () |> Store.makeElmishSimple (fun _ -> tabParent) Tabs.update ignore

    // let _,v = navRootArr


    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store ]
        Html.ul[Html.li[data_ "icon" "intentionally missing"
                        Bulma.FontAwesome.fa "mo"]

                Html.li [ Html.div [ tryIcon (App.Init.MuiIcon "Link") ] ]
                Html.li [ tryIcon (App.Init.FAIcon "intercom") ]

                Html.li [
                    Html.div [ data_ "icon" "fort-awesome"; tryIcon (App.Init.FAIcon "fort-awesome") ]
                ]]
        App.Components.Gen.Tabs.view (Choice2Of2 tabStore)
    ]
