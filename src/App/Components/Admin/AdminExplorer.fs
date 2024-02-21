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
open Core

module Style =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle

    // https://github.com/BulmaTemplates/bulma-templates/blob/master/css/admin.css
    let css = [
        rule "*" [ Css.lineHeight (rem 1.5); Css.height (percent 100); Css.marginLeft 0 ]
        rule ".adminExplorer" [ Css.backgroundColor "#ECF0F3" ]
        rule "nav.navbar" [ Css.borderTop (px 4., solid, "#276cda"); Css.marginBottom (rem 1.0) ]
        rule ".navbar-item.brand-text" [ Css.fontWeight 300 ]
        rule ".navbar-item, .navbar-link" [ Css.fontSize (px 14); Css.fontWeight 700 ]
        rule ".columns" [ Css.width (percent 100); Css.height (percent 100); Css.marginLeft 0 ]
        rule ".overlay" [
            Css.positionFixed
            Css.displayBlock
            Css.width (percent 100)
            Css.height (percent 100)
            Css.top 0
            Css.left 0
            Css.right 0
            Css.bottom 0
            Css.backgroundColor "rgba(0,0,0,0.5)"
            Css.zIndex 31
        ]

        rule ".bordered" [
            Css.border (px 2, solid, "#A9A9A9") // https://www.w3schools.com/colors/colors_shades.asp
            Css.custom ("border-radius", "0 0 .5rem .5rem")
        ]

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
        rule ".flyover" [
            Css.animationDuration (System.TimeSpan.FromMilliseconds 300)
            Css.animationTimingFunctionEaseIn
            Css.zIndex 32
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

type SelectedItemState =
    // a new folder won't have a child store?
    | FolderSelected of Choice<LazyNavItem, NavItem> * editing: bool
    | ChildSelected of LazyNavItem * NavItem

type SelectedItemType = SelectedItemState option

let getSelectedItem: SelectedItemType -> NavItem option =
    Option.map (function
        | FolderSelected(Choice1Of2 lni, _) -> lni.NavItem
        | FolderSelected(Choice2Of2 item, _) -> item
        | ChildSelected(_, ni) -> ni)


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
        {
            model with
                Item = Some(SelectedItemState.ChildSelected(parent, NavItem.CreateEmpty <| Some parent.NavItem.Path))
        },
        Cmd.none
    | Msg.StartNewRequested(None) ->
        {
            model with
                Item = Some(FolderSelected(Choice2Of2(NavItem.CreateEmpty None), false))
        },
        Cmd.none
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

        {
            model with
                Item = Some(FolderSelected(Choice1Of2 lni, false))
        },
        cmd

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

    // navItem |> (fun x -> x.Id) |> printfn "viewLeftNavItem: %A"

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
    let navItemButton (item: Choice<_, NavItem>) active disabled stateOpt =
        let name =
            match item with
            | Choice1Of2 lni -> lni.NavItem.Name
            | Choice2Of2 ni -> ni.Name

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
                // selecting this already-loaded parent
                let fSelect _ =
                    selectedItemStore.Update(fun _ -> Some(SelectedItemState.FolderSelected(item, true)))

                simpleNavItemButton name active (Some fSelect) []

    match childItemOpt with
    | Some child ->
        Bind.el (
            selectedItemStore,
            fun selectedItem ->
                let isActive =
                    selectedItem
                    |> getSelectedItem
                    |> Option.map (fun si -> si.Id = child.Id)
                    |> Option.defaultValue false

                let fSelect _ =
                    selectedItemStore.Update(fun _ -> Some(SelectedItemState.ChildSelected(root, child)))

                simpleNavItemButton child.Name isActive (Some fSelect) []
        )
    | None ->

        // scenarios:
        // root item, errored children
        // this is a root item, already selected and already with fetched children
        // this is a root item, already selected in flight
        // this is a root item, already selected but shows as not requested ?
        Bind.el2 root.ChildStore selectedItemStore (fun (childRemote, selectedItem) ->
            // printfn "Render item child store: %A" root.NavItem.Id

            let isSelected =
                getSelectedItem selectedItem
                |> Option.map (fun ni -> ni.Id = navItem.Id)
                |> Option.defaultValue false

            navItemButton (Choice2Of2 navItem) isSelected false (Some childRemote))

let viewLeftNav (items: LazyNavItem[]) selectedItemStore dispatch =
    printfn "Render viewLeftNav"

    Html.aside [
        Attr.className "menu is-hidden-mobile"
        columns2 [ Attr.className "bordered" ] [
            Html.select [
                Attr.className "select"
                Html.option [ text "" ]
                for item in items do
                    Html.option [
                        data_ "item" (Core.pretty item.NavItem)
                        Attr.value item.NavItem.Name
                        text item.NavItem.Name
                        Attr.title <| Core.pretty item.NavItem.Description
                        Bind.attr ("selected", selectedItemStore |> Store.map (fun selectedItem -> selectedItem))
                    ]

            ]

        ] [
            bButton "Add Item" [ "Add" |> App.Init.IconSearchType.MuiIcon |> Icons.tryIcon ]
        ]
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
                    // | Some(ChildSelected (_, ni)) -> liA (Some "isActive") ni.Name
                    | Some(FolderSelected(Choice1Of2 lni, enabled)) ->
                        Html.li [
                            Html.a [
                                text lni.NavItem.Name
                                onClick
                                    (fun _ ->
                                        selectedItemStore.Update(fun _ ->
                                            Some(FolderSelected(Choice1Of2 lni, not enabled))))
                                    []
                            ]
                        ]
                    | Some(FolderSelected(Choice2Of2 ni, _)) -> liA (Some "is-active") ni.Name
                    | Some(ChildSelected(lni, ni)) ->
                        Html.li [
                            Html.a [
                                text lni.NavItem.Name
                                onClick
                                    (fun _ ->
                                        selectedItemStore.Update(fun _ -> Some(FolderSelected(Choice1Of2 lni, false))))
                                    []
                            ]
                        ]

                        liA None ni.Name
                ]
        )
    ]

let view token =
    printfn "AdminExplorer"
    let store, dispatch = token |> Store.makeElmish init update ignore
    toGlobalWindow "adminExplorer_model" store.Value

    let getValueDisplay =
        function
        | None -> "null"
        | Some(FolderSelected(Choice1Of2 lni, editing)) -> $"FolderSelected(lni:%A{lni.NavItem.Name}, %A{editing})"
        | Some(FolderSelected(Choice2Of2 ni, editing)) -> $"FolderSelected(ni:%A{ni.Name}, %A{editing})"
        | Some(ChildSelected(lni, ni)) -> $"ChildSelected(lni:%s{lni.NavItem.Name}, ni:%s{ni.Name})"

    let selectedItemStore =
        let getter model = model.Item

        let setter nextValue =
            let oldValue = getter store.Value


            printfn "Setting selected item: %A -> %A" (getValueDisplay oldValue) (getValueDisplay nextValue)
            { store.Value with Item = nextValue }

        store |> Store.mapStore "AESelectedItem" (getter, setter)

    let dispose =
        selectedItemStore.Subscribe(fun value -> printfn "SelectedItemChanged to: %A" (getValueDisplay value))

    // selectedItemStore.Update(fun _ -> selectedItemStore.Value)
    // let breadCrumbStore =
    //     let getter model = model.Item
    //     let setter
    let itemObs =
        store
        |> Store.map (fun model ->
            model.Item
            |> Option.map (function
                | FolderSelected(Choice1Of2 lni, _) -> (Some lni, None)
                | FolderSelected(Choice2Of2 _, _) -> None, None
                | ChildSelected(lni, ni) -> (Some lni, Some ni))
            |> Option.defaultValue (None, None))

    Html.div [
        Attr.className "adminExplorer"
        disposeOnUnmount [ dispose ]
        Bind.el (
            selectedItemStore,
            function
            | Some(FolderSelected(_, false))
            | None ->
                printfn "Overlay render hide"
                Html.div [ Attr.id "overlay"; Attr.styleAppend [ Css.visibilityHidden ] ]
            // TODO: add left side props editor display
            | Some selectedItem ->
                printfn "Overlay render"

                Html.divc "overlay" [
                    Attr.id "overlay"
                    // leave folder selected but turn off editing if editing is on
                    onClick
                        (fun _ ->
                            printfn "Overlay click handler"

                            let next =
                                match selectedItem with
                                | SelectedItemState.FolderSelected(fs, true) ->
                                    printfn "Unselecting folder"
                                    FolderSelected(fs, false) |> Some
                                | _ -> None

                            selectedItemStore.Update(fun _ -> next))
                        []
                ]
        )
        Samples.viewNavBar ()
        viewBreadCrumbs selectedItemStore
        Html.divc "container" [
            Html.divc "columns" [
                Html.divc "column is-3" [
                    Bind.el (
                        store |> Store.map (fun model -> model.Items),
                        function
                        | None -> Html.div [ text "Loading..." ]
                        | Some(Ok items) ->
                            printfn "Render view left column"

                            viewLeftNav items selectedItemStore dispatch
                        | Some(Error e) ->
                            Html.div [
                                Attr.className "is-error"
                                App.Components.Gen.ErrorHandling.renderErrors [ e ]
                            ]
                    )
                ]
                // middle nav (root item child list)
                Html.divc "column is-3" [

                    Bind.el (
                        itemObs,
                        fun (parent, item) ->
                            printfn "Render middle column"

                            match parent with
                            | None -> Html.div []
                            | Some parent ->
                                printfn "Render children"

                                Bind.el (
                                    parent.ChildStore,
                                    function
                                    | InFlight -> Html.div [ text "Loading..." ]
                                    | NotRequested -> Html.divc "is-error" [ text "Not requested" ]
                                    | Responded(Error e) -> Html.divc "is-error" [ text <| string e ]
                                    | Responded(Ok(values, _)) ->
                                        Html.ul [
                                            yield!
                                                values
                                                |> Seq.map (fun v ->
                                                    let x = viewLeftNavItem selectedItemStore dispatch (parent, Some v)
                                                    x)

                                        ]

                                )
                    // Html.div [
                    //     viewLeftNavItem selectedItemStore dispatch (parent, item)
                    //     ]
                    )
                ]
                Html.divc "column is-6" [
                    let itemObs =
                        itemObs
                        |> Observable.map (fun (lni, ni) -> lni |> Option.map (fun lni -> lni.NavItem), ni)

                    Bind.el (
                        itemObs,
                        fun (parentOpt, childOpt) ->
                            match parentOpt, childOpt with
                            | None, None -> Html.div []
                            | _ ->
                                Html.sectionc "flyover hero is-info welcome is-small" [
                                    Html.divc "hero-body" [
                                        NavUI.view token { Parent = parentOpt; Item = childOpt }

                                    ]
                                ]
                    )

                ]
            ]
        ]
    ]
    |> Style.withCss
