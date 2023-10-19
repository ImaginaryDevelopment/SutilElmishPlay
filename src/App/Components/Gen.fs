module App.Components.Gen

open Sutil
open Sutil.Core
open Sutil.CoreElements

type ErrorType = exn

module CssRules =
    open Sutil.Styling

    let titleIndicator = [ // https://www.scientificpsychic.com/etc/css-mouseover.html
        Css.custom ("border-bottom", "thin dotted")
        Css.backgroundColor "#ffeedd"
        // Css.paddingLeft 5
        Css.marginLeft 5
        Css.cursorHelp
    ]

type RemoteData<'t> =
    | NotRequested
    | InFlight
    | Responded of Result<'t, ErrorType>

    static member TryGet(remoteData: RemoteData<'t>) =
        match remoteData with
        | Responded(Ok x) -> Some x
        | _ -> None

type RemoteMsg<'tReq, 'tResp> =
    | Request of 'tReq
    | Response of Result<'tResp, ErrorType>

type GenericModelArgs<'t, 'tModel, 'tMsg, 'tReqArgs> = {
    GetState: 'tModel -> RemoteData<'t>
    WrapMsg: RemoteMsg<'tReqArgs, 't> -> 'tMsg
}

type GenericFetchUpdateArgs<'t, 'tModel, 'tMsg, 'tFetchArg, 'tReqArgs> = {
    Gma: GenericModelArgs<'t, 'tModel, 'tMsg, 'tReqArgs>
    SetState: 'tModel -> RemoteData<'t> -> 'tModel
    GetArgs: 'tModel -> 'tFetchArg
    Fetch: 'tFetchArg -> Async<Result<'t, ErrorType>>
}

type GenericViewArgs<'t, 'tModel, 'tMsg, 'tReqArgs> = {
    Title: string
    Gma: GenericModelArgs<'t, 'tModel, 'tMsg, 'tReqArgs>
    GetObserver: IStore<'tModel> -> ('tModel -> RemoteData<'t>) -> System.IObservable<RemoteData<'t>>
    GetReqArg: 'tModel -> 'tReqArgs
}

module GenericFetcher =
    open Sutil.CoreElements
    open App.Adapters.Html

    let createUpdate gfa =
        let getCmd (args: 'tArg) =
            async {
                let! (resp: Result<'t, ErrorType>) = gfa.Fetch args
                return gfa.Gma.WrapMsg(Response resp)
            }

        let update msg (model: 'tModel) =
            match msg, gfa.Gma.GetState model with
            | Request _, InFlight -> model, Cmd.none
            | Request _, _ -> gfa.SetState model InFlight, Cmd.OfAsync.perform getCmd (gfa.GetArgs model) id
            | Response x, _ -> gfa.SetState model (Responded x), Cmd.none

        update

    // let createModule<'t> (fObs: _ -> _) (fState: Model -> RemoteData<'t>, fSetState: Model -> RemoteData<'t> -> Model) (fFetch: string -> Async<Result<'t,_>>) (fMsgWrap: RemoteMsg<_,'t> -> Msg) fCmd =
    let createView (gfa: GenericViewArgs<'t, 'tModel, 'tMsg, 'tFetchArg>) =

        let view (store, dispatch) =
            Bind.el (
                gfa.GetObserver store gfa.Gma.GetState,
                fun (mis: RemoteData<'t>) ->
                    let inline dispatchReqMsg _ =
                        store.Value |> gfa.GetReqArg |> RemoteMsg.Request |> gfa.Gma.WrapMsg |> dispatch

                    let buttonText = text gfa.Title

                    match mis with
                    | RemoteData.NotRequested -> Html.button [ buttonText; onClick dispatchReqMsg [] ]
                    | RemoteData.InFlight -> Html.button [ Attr.disabled true; buttonText ]
                    | RemoteData.Responded(Ok(data)) ->
                        Html.div [
                            Html.pre [
                                // prop.custom("data-status","200")
                                data_ "status" "200"
                                text (Core.pretty data)
                            ]
                        ]
                    | RemoteData.Responded(Error exn) -> Html.divc "error" [ text (Core.pretty exn) ]
            )

        view

module ErrorHandling =
    let renderErrorDisplay obs =
        Bind.el (
            obs,
            function
            | [] -> Html.divc "errors" []
            | errors ->
                Html.divc "errors" [
                    Html.ul [
                        for (e, dt) in errors do
                            Html.li [ text e ]
                    ]
                ]
        )

module Icons =

    open Sutil.Styling
    open type Feliz.length

    let css = [
        // rule based on nav menu
        rule "svg.mui" [
            // Css.maxWidth (px 24)
            // Css.maxHeight (px 24)
            Css.height (em 1.0)
            Css.width (em 1.0)
            Css.flexShrink 0
            Css.fontSize (rem 1.5)
            Css.color ("rgb(61, 60, 65)")
        // Css.padding (px 15.0)
        ]
    ]

    let tryIcon x =
        // bulma icon class
        Html.spanc "icon" [
            match App.Init.icon x, x with
            | None, App.Init.IconSearchType.FAIcon x
            | None, App.Init.IconSearchType.MuiIcon x ->
                // text $"missing:{x}"
                Bulma.FontAwesome.fa x
            | Some(App.Init.FaResult v), _ ->
                if v.html.Length <> 1 then
                    eprintfn "Unexpected fa html len: %i" v.html.Length

                let html = v.html[0]
                Html.parse html
            | Some(App.Init.MuiResult dPath), _ ->

                Svg.svg [ Attr.className "mui"; Svg.path [ Attr.d dPath ] ] |> withStyle css
        ]

// https://svelte.dev/repl/cf05bd4a4ca14fb8ace8b6cdebbb58da?version=3.17.0
type Tab = {
    Label: string
    // consider trying this without a value, let the index in the parent dictate
    Value: int
    Component: SutilElement
}

type TabParent = {
    Name: string
    ActiveTab: int
    Tabs: Tab[]
}

// Svelte non-bulma tabs
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
            Css.padding 20
            Css.border (px 1., solid, gColor)
            Css.custom ("border-radius", "0 0 .5rem .5rem")
            Css.borderTopWidth 0
            Css.width (perc 100.0)
        ]

        rule "ul" [
            Css.displayFlex
            Css.flexWrapWrap
            Css.paddingLeft 0
            Css.marginBottom 0
            Css.listStyleTypeNone
            Css.borderBottom (px 1., solid, gColor)
        ]

        rule "li" [ Css.marginBottom (px -1.) ]

        rule "span" [
            Css.border (px 1.0, solid, "transparent")
            Css.custom ("border-top-left-radius", "0.25rem")
            Css.custom ("border-top-right-radius", "0.25rem")
            Css.displayBlock
            Css.padding (rem 0.5, rem 1.0)
            Css.cursor "pointer"
        ]

        rule "span:hover" [ Css.borderColor $"#e9ecef #e9ecef {gColor}" ]

        rule "li.active>span" [
            Css.color "#495057"
            Css.backgroundColor "#fff"
            Css.borderColor $"{gColor} {gColor} #fff"
        ]
    ]

    type Msg = TabClick of int

    let update msg (model: TabParent) =
        printfn "Tab update: %s" model.Name

        match msg with
        | TabClick i -> { model with ActiveTab = i }

    let private viewOk (store: IStore<TabParent>) tabValue dispatch =
        printfn "viewOk:%i" tabValue

        let menu =
            Html.ul [
                data_ "t" "TabView"
                for tab in store.Value.Tabs do
                    Html.li [
                        if tab.Value = tabValue then
                            Attr.className "active"
                        Html.span [
                            if tab.Value <> tabValue then
                                onClick (fun _ -> Msg.TabClick tab.Value |> dispatch) []
                            text tab.Label
                        ]
                    ]
            ]

        Html.div [
            menu
            Html.ul [
                for tab in store.Value.Tabs do
                    if tabValue = tab.Value then
                        Html.li [ Attr.className "box"; tab.Component ]
            ]
        ]

    let private viewDupes (model: TabParent) dupes =
        Html.ul [
            data_ "name" model.Name
            for dupe in dupes do
                Html.li [ text dupe ]
        ]
    // trying to let the parent hold the store, to see if that helps with data loss on rerender
    let view (store: Choice<TabParent, IStore<TabParent> * Dispatch<Msg>>) =
        let findDupes (tabs: Tab[]) =
            tabs
            |> Seq.groupBy (fun v -> v.Value)
            |> Seq.map (fun (g, v) -> g, v |> List.ofSeq)
            |> Seq.filter (fun (g, v) -> v.Length > 1)
            |> Seq.collect snd
            |> Seq.map (fun v -> v.Label)
            |> List.ofSeq

        let store, dispatch =
            match store with
            | Choice1Of2 tp ->
                let result = () |> Store.makeElmishSimple (fun _ -> tp) update ignore
                result
            | Choice2Of2 store -> store

        match findDupes store.Value.Tabs with
        | [] ->
            Bind.el (store |> Store.map (fun v -> v.ActiveTab), (fun tabValue -> viewOk store tabValue dispatch))
            |> withStyle css
        | dupes -> viewDupes store.Value dupes
