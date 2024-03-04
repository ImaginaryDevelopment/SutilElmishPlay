module App.Components.Diag

open Sutil
open Sutil.CoreElements

open BReusable

open App.Adapters.Config
open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen
open App.Adapters.Icons
open App.Adapters.Bulma
open App.Adapters.Api.Schema
open App.Adapters.Api.Shared
open App.Adapters.Schema


type RemoteStates = {
    MyInfoState: RemoteData<MyInfoResponse>
    NavRootState: RemoteData<NavItem[]>
    AclState: RemoteData<AclType[]>
    AclParamSearchState: RemoteData<AclSearchResult>
}

type Model = {
    AppMode: ConfigType<string>
    States: RemoteStates
    AclParamSearchInput: AclRefValueArgs
}

let dummyData: NavItem[] =
    Array.ofList [
        {
            Id = NavId "1"
            Path = "Path"
            Parent = "Parent"
            Type = Link
            Name = "Name"
            Description = "Description"
            Enabled = false
            Icon = "user"
            Weight = 0
            Url = "url"
            HasUrlKey = false
            AclRefs = Map.empty
        }
    ]

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
    | Acl of RemoteMsg<unit, AclType[]>
    | AclParam of RemoteMsg<AclRefValueArgs, AclSearchResult>

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
        AclParamSearchInput = {
            AclName = AclName ""
            SearchText = ""
            Max = None
        }
    },
    Cmd.none

module Commands =
    let getMyInfo token : Cmd<Msg> =
        let f () =
            async {
                let! resp = getMyInfo token
                let resp = resp |> Result.mapError Choice2Of2
                return Msg.MyInfo(Response resp)
            }

        Cmd.OfAsync.perform f () id

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
                | ConfigType.Auth token ->
                    App.Adapters.Api.Mapped.NavItems.getNavRoot token ()
                    |> Async.map (Result.mapError Choice2Of2)
                | ConfigType.Demo -> Async.ofValue (Ok dummyData)
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
                | Demo -> Async.ofValue (Ok Array.empty)
                | Auth token ->
                    App.Adapters.Api.Mapped.getAclTypes token ()
                    |> Async.map (Result.mapError Choice2Of2) // 'tFetchArg -> Async<Result<'t,ErrorType>>
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
                | Auth token, si -> searchAclRefValues token si |> Async.map (Result.mapError Choice2Of2)
                | Demo, _ -> Async.ofValue (Error <| Choice2Of2(System.Exception("Not Implemented")))
        }

    let view =
        Gen.GenericFetcher.createView {
            Title = "Acl Search"
            Gma = gma
            GetObserver = fun store f -> store |> Store.map f
            GetReqArg = fun model -> model.AclParamSearchInput
        }

    update, view

module Renderers =
    let renderApiTabs store dispatch =
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
                    MyInfoState = Responded(Error(Choice2Of2(System.Exception("Not Implemented"))))
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


()

let view dia =
    let (store: IStore<Model>, dispatch) = dia |> Store.makeElmish init update ignore

    Core.toGlobalWindow "diag_model" store.Value

    let tabStore =
        let tabParent = Renderers.renderApiTabs store dispatch

        () |> Store.makeElmishSimple (fun _ -> tabParent) Tabs.update ignore

    // let _,v = navRootArr


    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ store ]
        // renderTabs [

        // ]
        Html.divc "box" [

            Html.ul[Html.li[data_ "icon" "intentionally missing"
                            Bulma.FontAwesome.fa "mo"]

                    Html.li [ Html.divc "icon" [ Html.div [ tryIcon (MuiIcon "Link") ] ] ]
                    Html.li [ Html.divc "icon" [ tryIcon (FAIcon "intercom") ] ]

                    Html.li [
                        Html.divc "icon" [ Html.div [ data_ "icon" "fort-awesome"; tryIcon (FAIcon "fort-awesome") ] ]
                    ]]
        ]
        App.Components.Gen.Tabs.view (Choice2Of2 tabStore)
    ]
