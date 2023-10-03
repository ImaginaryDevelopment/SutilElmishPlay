module App.Components.Gen

open Sutil
open Sutil.Core

type ErrorType = exn

type RemoteData<'t> =
    | NotRequested
    | InFlight
    | Responded of Result<'t,ErrorType>

type RemoteMsg<'tReq, 'tResp> =
    | Request of 'tReq
    | Response of Result<'tResp,ErrorType>

type GenericModelArgs<'t,'tModel, 'tMsg, 'tReqArgs> = {
    GetState: 'tModel -> RemoteData<'t>
    WrapMsg: RemoteMsg<'tReqArgs,'t> -> 'tMsg
}

type GenericFetchUpdateArgs<'t,'tModel, 'tMsg, 'tFetchArg, 'tReqArgs> = {
    Gma: GenericModelArgs<'t,'tModel,'tMsg, 'tReqArgs>
    SetState: 'tModel -> RemoteData<'t> -> 'tModel
    GetArgs: 'tModel -> 'tFetchArg
    Fetch: 'tFetchArg -> Async<Result<'t,ErrorType>>
}

type GenericViewArgs<'t, 'tModel, 'tMsg, 'tReqArgs> = {
    Title: string
    Gma: GenericModelArgs<'t,'tModel,'tMsg, 'tReqArgs>
    GetObserver: IStore<'tModel> -> ('tModel -> RemoteData<'t>) -> System.IObservable<RemoteData<'t>>
    GetReqArg: 'tModel -> 'tReqArgs
}

module GenericFetcher =
    open Sutil.CoreElements
    open App.Adapters.Html

    let createUpdate gfa =
        let getCmd (args: 'tArg) =
            async{
                let! (resp : Result<'t,ErrorType>) = gfa.Fetch args
                return gfa.Gma.WrapMsg(Response resp)
            }

        let update msg (model: 'tModel) =
            match msg, gfa.Gma.GetState model with
            | Request _, InFlight -> model, Cmd.none
            | Request _, _ ->  gfa.SetState model InFlight, Cmd.OfAsync.perform getCmd (gfa.GetArgs model) id
            | Response x, _ -> gfa.SetState model (Responded x), Cmd.none
        update

    // let createModule<'t> (fObs: _ -> _) (fState: Model -> RemoteData<'t>, fSetState: Model -> RemoteData<'t> -> Model) (fFetch: string -> Async<Result<'t,_>>) (fMsgWrap: RemoteMsg<_,'t> -> Msg) fCmd =
    let createView (gfa:GenericViewArgs<'t,'tModel, 'tMsg, 'tFetchArg>) =

        let view (store, dispatch) =
            Bind.el(gfa.GetObserver store gfa.Gma.GetState, fun (mis:RemoteData<'t>) ->
                let inline dispatchReqMsg _ = store.Value |> gfa.GetReqArg |> RemoteMsg.Request |> gfa.Gma.WrapMsg |> dispatch
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
        view

// https://svelte.dev/repl/cf05bd4a4ca14fb8ace8b6cdebbb58da?version=3.17.0
type Tab = {
    Label: string
    // consider trying this without a value, let the index in the parent dictate
    Value: int
    Component: SutilElement
}

type TabParent = {
    ActiveTab: int
    Tabs: Tab[]
}

module Tabs =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle
    open Sutil.CoreElements
    open App.Adapters.Html


    let css = [
        let gColor = "#dee2e6"
        rule ".box" [
            Css.marginBottom 10
            Css.padding 40
            Css.border(px 1., solid, gColor)
            Css.custom("border-radius","0 0 .5rem .5rem")
            Css.borderTopWidth 0
        ]
        rule "ul" [
            Css.displayFlex
            Css.flexWrapWrap
            Css.paddingLeft 0
            Css.marginBottom 0
            Css.listStyleTypeNone
            Css.borderBottom(px 1., solid, gColor)
        ]
        rule "li" [
            Css.marginBottom( px -1. )
        ]
        rule "span" [
            Css.border( px 1.0, solid, "transparent")
            Css.custom("border-top-left-radius","0.25rem")
            Css.custom("border-top-right-radius","0.25rem")
            Css.displayBlock
            Css.padding(rem 0.5, rem 1.0)
            Css.cursor "pointer"
        ]
        rule "span:hover" [
            Css.borderColor $"#e9ecef #e9ecef {gColor}"
        ]
        rule "li.active > span" [
            Css.color "#495057"
            Css.backgroundColor "#fff"
            Css.borderColor $"{gColor} {gColor} #fff"
        ]
    ]

    type Msg =
        | TabClick of int

    let update msg model =
        match msg with
        | TabClick i -> {model with ActiveTab=i}
    // trying to let the parent hold the store, to see if that helps with data loss on rerender
    let view (store:Choice<TabParent,IStore<TabParent>*Dispatch<Msg>>) =
        let store, dispatch =
            match store with
            | Choice1Of2 tp ->
                let result = () |> Store.makeElmishSimple (fun _ -> tp) update ignore
                result
            | Choice2Of2 store -> store
        Bind.el(store |> Store.map(fun v -> v.ActiveTab), fun tabValue ->
            let menu =
                Html.ul [
                    data_ "t" "TabView"
                    for tab in store.Value.Tabs do
                        Html.li [
                            if tab.Value = tabValue then
                                Attr.className "active"
                            Html.span [
                                if tab.Value <> tabValue then onClick (fun _ -> Msg.TabClick tab.Value |> dispatch) []
                                text tab.Label
                            ]
                        ]
                ]
            Html.div [
                menu
                Html.ul [
                    for tab in store.Value.Tabs do
                        if tabValue = tab.Value then
                            Html.li [
                                tab.Component
                            ]
                ]
            ]
        )
        |> withStyle css
