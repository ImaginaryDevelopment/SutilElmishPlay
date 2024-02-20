module App.Components.Admin.Explorer

// https://github.com/BulmaTemplates/bulma-templates/blob/master/templates/admin.html
// demo https://bulmatemplates.github.io/bulma-templates/templates/admin.html


open Sutil
open Sutil.Core
open Sutil.CoreElements

open App.Adapters.Schema
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api.Schema

open App.Components.Gen

module Style =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle

    // https://github.com/BulmaTemplates/bulma-templates/blob/master/css/admin.css
    let css = [
        rule "*" [
            Css.lineHeight (rem 1.5)
            Css.height (percent 100)
            Css.marginLeft 0
        // Css.backgroundColor "#ECF0F3"
        ]
        rule "nav.navbar" [ Css.borderTop (px 4., solid, "#276cda"); Css.marginBottom (rem 1.0) ]
        rule ".navbar-item.brand-text" [ Css.fontWeight 300 ]
        rule ".navbar-item, .navbar-link" [ Css.fontSize (px 14); Css.fontWeight 700 ]
        rule ".columns" [ Css.width (percent 100); Css.height (percent 100); Css.marginLeft 0 ]
        rule ".menu-label" [
            Css.color "#8F99A3"
            Css.custom ("letter-spacing", "1.3")
            Css.fontWeight 700
        ]
        rule ".menu-list a" [ Css.color "#8F99A3"; Css.fontSize (px 14); Css.fontWeight 700 ]
        rule ".menu-list a:hover, .menu-label:hover" [
            Css.backgroundColor "transparent"
            Css.color "#276cda"
            Css.cursorPointer
        ]
        rule ".menu-list a.is-active, .menu-label.is-active" [
            Css.backgroundColor "transparent"
            Css.color "#276cda"
            Css.fontWeight 700
        ]
    ]

    let withCss = withStyle css

type AdminErrorType = string * System.DateTime

let translateErrorType: ErrorType -> AdminErrorType =
    function
    | Choice1Of2 e -> e |> String.concat ",", System.DateTime.Now
    | Choice2Of2 e -> string e, System.DateTime.Now

type LazyNavItem = {
    NavItem: NavItem
    ChildStore: IStore<RemoteData<NavItem[] * bool>>
}

type SelectedItemType = (LazyNavItem * NavItem option) option

let getSelectedItem: SelectedItemType -> NavItem option =
    function
    | None -> None
    | Some(lni, None) -> Some lni.NavItem
    | Some(_, Some item) -> Some item


type Model = {
    Token: string
    Items: Result<LazyNavItem[], AdminErrorType> option
    // must track parent for bread crumbs
    Item: SelectedItemType
    Errors: AdminErrorType list
}

let private liA aClsOpt linkText =
    Html.li [
        Html.a [
            match aClsOpt with
            | None -> ()
            | Some(cls: string) -> Attr.className cls
            text linkText
        ]
    ]

module Samples =
    let viewNavBar () =
        Html.navc "navbar is-white" [
            Html.divc "container" [
                (*
                <div class="navbar-brand">
                    <a class="navbar-item brand-text" href="../index.html"> Bulma Admin </a>
                    <div class="navbar-burger burger" data-target="navMenu">
                        <span></span>
                        <span></span>
                        <span></span>
                    </div>
                </div>
                *)
                Html.divc "navbar-brand" [
                    Html.a [ text "Admin Explorer" ]
                    Html.divc "navbar-burger burger" [
                        data_ "target" "navMenu"
                        Html.span []
                        Html.span []
                        Html.span []
                    ]
                ]
                Html.divc "navbar-menu" [
                    Attr.id "navMenu"
                    Html.divc "navbar-start" [
                        Html.a [ Attr.className "navbar-item"; Attr.href "#"; text "Home" ]
                        Html.a [ Attr.className "navbar-item"; Attr.href "#"; text "Orders" ]
                        Html.a [ Attr.className "navbar-item"; Attr.href "#"; text "Payments" ]
                    ]
                ]
            ]
        ]

    let viewLeftNav () =

        fragment [
            Html.aside [
                Attr.className "menu is-hidden-mobile"
                Html.pc "menu-label" [ text "General" ]
                Html.ulc "menu-list" [ liA (Some "is-active") "Dashboard"; liA None "Customers"; liA None "Other" ]
                Html.pc "menu-label" [ text "Administration" ]
                Html.ul [
                    Attr.className "menu-list"
                    liA None "Team Settings"
                    Html.li [
                        Html.a [ text "Manage Your Team" ]
                        Html.ul [
                            liA None "Members"
                            liA None "Plugins"
                            liA None "Add a member"
                            liA None "Remove a member"
                        ]

                    ]
                    liA None "Invitations"
                    liA None "Cloud Storage Environment Settings"
                    liA None "Authentication"
                    liA None "Payments"
                ]
                Html.pc "menu-label" [ text "Transactions" ]
                Html.ul [
                    Attr.className "menu-list"
                    liA None "Payments"
                    liA None "Transfers"
                    liA None "Balance"
                    liA None "Reports"
                ]
            ]
        ]

    let viewBreadCrumbs () =
        Html.navc "breadcrumb" [
            Attr.custom ("aria-label", "breadcrumbs")
            Html.ul [
                Html.li [ Html.a [ text "Bulma" ] ]
                liA None "Templates"
                liA None "Examples"
                liA (Some "is-active") "Admin"
            ]
        ]

    let view () =
        Html.div [
            // Attr.className "leftMe"
            viewNavBar ()
            Html.divc "container" [
                Html.divc "columns" [
                    Html.divc "column is-3" [ viewLeftNav () ]
                    Html.divc "column is-9" [
                        viewBreadCrumbs ()
                        Html.sectionc "hero is-info welcome is-small" [
                            Html.divc "hero-body" [
                                Html.divc "container" [

                                    Html.h1 [ Attr.className "title"; text "Hello, Admin." ]
                                    Html.h2 [ Attr.className "subtitle"; text "I Hope you are having a great day!" ]
                                ]
                            ]
                        ]

                    ]
                ]
            ]
        ]

type Msg =
    | RootResponse of Result<NavItem[], ErrorType>
    | PathResponse of LazyNavItem * Result<NavItem[], ErrorType>
    // | PathToggle of LazyNavItem * expand: bool
    | PathRequested of LazyNavItem
    // should they be able to create folders?
    // if so, then this should be an option
    | StartNewRequested of parent: LazyNavItem option


module Cmd =
    let getRootItems token : Cmd<Msg> =
        let f x =
            async {
                let! resp = App.Adapters.Api.Mapped.NavItems.getNavRoot token x

                match resp with
                | Ok v -> return Msg.RootResponse(Ok v)
                | Error e -> return Msg.RootResponse(Error(Choice2Of2 e))
            }

        Cmd.OfAsync.either f () id (fun ex -> Msg.RootResponse(Error(Choice2Of2 ex)))

    let getRootChildren token (lni: LazyNavItem) : Cmd<Msg> =
        printfn "Maybe fetching root children"

        match lni.ChildStore.Value with
        | Responded _
        | NotRequested ->
            let f x =
                async {
                    printfn "Fetching root children: %A" lni.NavItem.Id
                    let! resp = App.Adapters.Api.Mapped.NavItems.getNavPath token x

                    match resp with
                    | Ok v -> return Msg.PathResponse(lni, Ok(v.Items))
                    | Error e -> return Msg.PathResponse(lni, Error(Choice2Of2 e))
                }

            Cmd.OfAsync.either f lni.NavItem.Path id (fun ex -> Msg.PathResponse(lni, Error(Choice2Of2 ex)))

        | InFlight -> Cmd.none

let init token : Model * Cmd<Msg> =
    let next = {
        Token = token
        Items = None
        Item = None
        Errors = List.empty
    }

    next, Cmd.getRootItems token

let private update msg (model: Model) : Model * Cmd<Msg> =
    printfn "AdminExplorer update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    // TODO: fix the Item selection to allow no parent
    | Msg.StartNewRequested(Some parent) ->
        // (LazyNavItem * NavItem option) option
        { model with Item = Some(parent, None) }, Cmd.none
    | Msg.RootResponse(Ok v) ->

        let nextItems =
            v
            |> Array.map (fun navItem ->
                let childStore = Store.make NotRequested

                {
                    NavItem = navItem
                    ChildStore = childStore
                })

        {
            model with
                Items = Some(Ok nextItems)
        },
        Cmd.none

    | Msg.RootResponse(Error e) ->
        Core.log e

        {
            model with
                Items = Some((string e, System.DateTime.Now) |> Error)
        },
        Cmd.none

    | Msg.PathResponse(lni, Ok children) ->
        lni.ChildStore.Update(fun _ -> Responded(Ok(children, true)))
        model, Cmd.none

    | Msg.PathResponse(lni, Error e) ->
        lni.ChildStore.Update(fun _ -> Responded(Error(e)))
        model, Cmd.none

    | Msg.PathRequested lni ->
        let mutable cmd = Cmd.none
        // updating a store property, not the model directly
        model.Items
        |> Option.iter (function
            | Ok v ->
                v
                |> Array.iter (fun item ->
                    if item.NavItem.Id = lni.NavItem.Id then
                        // TODO: update item
                        match item.ChildStore.Value with
                        | Responded _
                        | NotRequested ->
                            cmd <- Cmd.getRootChildren model.Token item

                            match item.ChildStore.Value with
                            | NotRequested -> item.ChildStore.Update(fun oldItem -> InFlight)
                            | _ -> ()
                        | InFlight -> ())
            | _ -> ())

        model, cmd

// module Renders =
//     let renderItemAsList item dispatch selectedItemStore =
// let (|RootErr|RootReady|RootOther|Child| ) navItem (childRemote, selectedItem: SelectedItemType) =
//     let isSelected = selectedItem |> getSelectedItem |> Option.map(fun v -> v.Id = navItem.Id)
//     match childRemote with


let viewLeftNavItem
    (selectedItemStore: IStore<SelectedItemType>)
    dispatch
    (root: LazyNavItem, childItemOpt: NavItem option)
    =
    let navItem = childItemOpt |> Option.defaultValue root.NavItem

    navItem |> (fun x -> x.Id) |> printfn "viewLeftNavItem: %A"

    let simpleNavItemButton name active fActivateOpt children =
        bButton "Select Item" [
            text name
            // will this add button to is-active or overwrite?
            if active then
                Attr.className "is-active"
            else
                match fActivateOpt with
                | None -> Attr.disabled true
                | Some fActivate -> onClick (fun _ -> fActivate ()) []
            yield! children
        ]


    // stateOpt = None assumes child
    let navItemButton name active disabled stateOpt =
        if disabled then
            simpleNavItemButton name active None []
        else
            let reqData _ = Msg.PathRequested root |> dispatch

            match stateOpt with
            | Some InFlight -> simpleNavItemButton name active None [ data_ "InFlight" "true" ]
            | Some NotRequested -> simpleNavItemButton name active (Some reqData) []
            | Some(Responded(Error e)) ->
                // allow data to be requested again when it fails
                simpleNavItemButton name active (Some reqData) [
                    match e with
                    | Choice1Of2 e -> data_ "error" <| String.concat "," e
                    | Choice2Of2 e -> data_ "error" e.Message
                ]
            | None
            | Some(Responded(Ok _)) ->
                let fSelect _ =
                    selectedItemStore.Update(fun _ -> Some(root, childItemOpt))

                simpleNavItemButton name active (Some fSelect) []

    match childItemOpt with
    | Some child ->
        Bind.el (
            selectedItemStore,
            fun selectedItem ->
                let isActive =
                    selectedItem
                    |> Option.bind snd
                    |> Option.map (fun si -> si.Id = child.Id)
                    |> Option.defaultValue false

                let fSelect _ =
                    selectedItemStore.Update(fun _ -> Some(root, childItemOpt))

                simpleNavItemButton child.Name isActive (Some fSelect) []
        )
    | None ->

        // scenarios:
        // root item, errored children
        // this is a root item, already selected and already with fetched children
        // this is a root item, already selected in flight
        // this is a root item, already selected but shows as not requested ?
        Bind.el2 root.ChildStore selectedItemStore (fun (childRemote, selectedItem) ->
            printfn "Render item child store: %A" root.NavItem.Id

            let isSelected =
                selectedItem
                |> Option.map (fun (lni, childOpt) -> childOpt |> Option.defaultValue lni.NavItem)
                |> Option.map (fun ni -> ni.Id = navItem.Id)
                |> Option.defaultValue false

            navItemButton navItem.Name isSelected false (Some childRemote))

let viewLeftNav (items: LazyNavItem[]) selectedItemStore dispatch =
    printfn "Render viewLeftNav"

    Html.aside [
        Attr.className "menu is-hidden-mobile"
        yield!
            items
            |> Seq.filter (fun item -> item.NavItem.Type = NavItemType.Folder)
            |> Seq.map (fun lni -> viewLeftNavItem selectedItemStore dispatch (lni, None))
    ]

let viewBreadCrumbs (selectedItemStore: IStore<SelectedItemType>) =
    Html.navc "breadcrumb" [
        Attr.custom ("aria-label", "breadcrumbs")
        Bind.el (
            selectedItemStore,
            fun _ ->
                Html.ul [
                    Html.li [ text "Root" ]
                    match selectedItemStore.Value with
                    | None -> ()
                    | Some(lni, None) -> liA (Some "is-active") lni.NavItem.Name
                    | Some(lni, Some ni) ->
                        Html.li [
                            Html.a [
                                text lni.NavItem.Name
                                onClick (fun _ -> selectedItemStore.Update(fun _ -> Some(lni, None))) []
                            ]
                        ]

                        liA None ni.Name
                ]
        )
    ]

let view token =
    printfn "AdminExplorer"
    let store, dispatch = token |> Store.makeElmish init update ignore

    let selectedItem =
        let getter model = model.Item

        let setter nextValue = { store.Value with Item = nextValue }
        store |> Store.mapStore "AESelectedItem" (getter, setter)
    // let breadCrumbStore =
    //     let getter model = model.Item
    //     let setter
    Html.div [
        Samples.viewNavBar ()
        Html.divc "container" [
            Html.divc "columns" [
                Html.divc "column is-3" [
                    Bind.el (
                        store |> Store.map (fun model -> model.Items),
                        function
                        | None -> Html.div [ text "Loading..." ]
                        | Some(Ok items) ->
                            printfn "Render view left column"

                            viewLeftNav items selectedItem dispatch
                        | Some(Error e) ->
                            Html.div [
                                Attr.className "is-error"
                                App.Components.Gen.ErrorHandling.renderErrors [ e ]
                            ]
                    )
                ]
                // middle nav (root item child list)
                Html.divc "column is-3" [
                    let itemObs =
                        store
                        |> Store.map (fun model ->
                            model.Item
                            |> Option.map (function
                                | lni, None -> (Some lni, None)
                                | _, Some ni -> (None, Some ni))
                            |> Option.defaultValue (None, None))

                    Bind.el (
                        itemObs,
                        fun (parent, item) ->
                            match parent with
                            | None -> Html.div []
                            | Some parent ->
                                Html.div [
                                    viewLeftNavItem selectedItem dispatch (parent, item)

                                ]
                    )
                ]
                Html.divc "column is-6" [
                    viewBreadCrumbs selectedItem
                    Html.sectionc "hero is-info welcome is-small" [
                        Html.divc "hero-body" [
                            let itemObs =
                                store
                                |> Store.map (fun model ->
                                    model.Item
                                    |> Option.map (function
                                        | lni, None -> (Some lni.NavItem, None)
                                        | _, Some ni -> (None, Some ni))
                                    |> Option.defaultValue (None, None))

                            let f (parent, child) =
                                NavUI.view token { Parent = parent; Item = child }

                            Bind.el (itemObs, f)
                        ]
                    ]

                ]
            ]
        ]
    ]
    |> Style.withCss
