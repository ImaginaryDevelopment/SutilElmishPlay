module App.Components.Admin.Explorer

// https://github.com/BulmaTemplates/bulma-templates/blob/master/templates/admin.html
// demo https://bulmatemplates.github.io/bulma-templates/templates/admin.html

open BReusable

open Sutil
open Sutil.Core
open Sutil.CoreElements

open App.Adapters.Schema
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api.Schema

open App.Components.Gen
open Core


type AdminErrorType = string * System.DateTime

let translateErrorType: ErrorType -> AdminErrorType =
    function
    | Choice1Of2 e -> e |> String.concat ",", System.DateTime.Now
    | Choice2Of2 e -> string e, System.DateTime.Now

type LazyNavItem = {
    NavItem: NavItem
    ChildStore: IStore<RemoteData<NavItem[] * bool>>
}

type FolderSelectionType =
    | NewFolder of NavItem
    | Existing of LazyNavItem * editing: bool

type SelectedItemState =
    // a new folder won't have a child store?
    | FolderSelected of FolderSelectionType
    | ChildSelected of LazyNavItem * NavItem

type SelectedItemType = SelectedItemState option

let getSelectedItem: SelectedItemType -> NavItem option =
    Option.map (function
        | FolderSelected(Existing(lni, _)) -> lni.NavItem
        | FolderSelected(NewFolder item) -> item
        | ChildSelected(_, ni) -> ni)

type Model = {
    Token: string
    Items: Result<LazyNavItem[], AdminErrorType> option
    CreateParentSelector: LazyNavItem option
    AclTypes: AclType[]
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

module Style =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle

    // https://github.com/BulmaTemplates/bulma-templates/blob/master/css/admin.css
    let css =
        // navbar is ~30
        let zOverlay = 31

        [
            rule "*" [ Css.lineHeight (rem 1.5); Css.height (percent 100); Css.marginLeft 0 ]
            rule ".adminExplorer" [ Css.backgroundColor "#ECF0F3" ]
            rule "nav.navbar" [ Css.borderTop (px 4., solid, "#276cda"); Css.marginBottom (rem 1.0) ]
            rule ".navbar-item.brand-text" [ Css.fontWeight 300 ]
            rule ".navbar-item, .navbar-link" [ Css.fontSize (px 14); Css.fontWeight 700 ]
            rule ".columns" [ Css.width (percent 100); Css.height (percent 100); Css.marginLeft 0 ]
            rule "#middle-child" [ Css.displayFlex; Css.custom ("flex-flow", "row wrap") ]
            rule "#middle-child>li" [
                // Css.flexBasisInitial
                // Css.flexBasisAuto
                Css.custom ("flex-basis", "50%")

            ]

            // not working properly or the target isn't rising properly
            rule ".overlay" [
            // Css.positionFixed
            // // Css.displayBlock
            // Css.width (percent 100)
            // Css.height (percent 100)
            // Css.top 0
            // Css.left 0
            // Css.right 0
            // Css.bottom 0
            // Css.backgroundColor "rgba(0,0,0,0.5)"
            // Css.zIndex zOverlay
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
            rule ".above-overlay" [ Css.zIndex <| zOverlay + 3 ]

            rule ".flyover" [
                Css.positionFixed
                Css.top 0
                // start off screen
                Css.right (percent -25)
                // adjust as needed
                Css.width (percent 50)
                Css.height (vh 100)
                Css.transition ("right 0.3s ease")

                // Css.animationDuration (System.TimeSpan.FromMilliseconds 700)
                // Css.animationTimingFunctionEaseIn
                Css.zIndex <| zOverlay + 5
                Css.backgroundColor "black"
                Css.paddingLeft (px 10)
            ]

            rule ".flyover.active" [ Css.right 0 ]
        ]

    let withCss = withStyle css

module MLens =

    let addError title (x: Model) = {
        x with
            Errors = (title, System.DateTime.Now) :: x.Errors
    }

    let addExnError title (ex: exn) x = addError $"%s{title}: %s{ex.Message}" x

    let addChcError title (choice: Choice<_, exn>) x =
        match choice with
        | Choice1Of2 errStr -> x |> addError $"%s{title}: %A{errStr}"
        | Choice2Of2 exn -> x |> addExnError title exn

type Msg =
    | RootResponse of Result<NavItem[], ErrorType>
    | PathResponse of LazyNavItem * Result<NavItem[], ErrorType>
    // | PathToggle of LazyNavItem * expand: bool
    | PathRequested of LazyNavItem
    // should they be able to create folders?
    // if so, then this should be an option
    | StartNewRequested of parent: LazyNavItem option
    | DeleteRequested of Choice<LazyNavItem, NavItem>
    | DeleteResponse of Result<NavItem, ErrorType>
    | AclTypeResponse of Result<AclType[], ErrorType>

// allow for pathRequest, and direct store selection update
// let getActivateItemAttrs dispatch

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
            let f x : Async<Msg> =
                async {
                    printfn "Fetching root children: %A" lni.NavItem.Id
                    let! resp = App.Adapters.Api.Mapped.NavItems.getNavPath token x

                    match resp with
                    | Ok v -> return Msg.PathResponse(lni, Ok(v.Items))
                    | Error e -> return Msg.PathResponse(lni, Error(Choice2Of2 e))
                }

            Cmd.OfAsync.either f lni.NavItem.Path id (fun ex -> Msg.PathResponse(lni, Error(Choice2Of2 ex)))

        | InFlight -> Cmd.none

    let getAclTypes token : Cmd<Msg> =
        let f x =
            App.Adapters.Api.Mapped.getAclTypes token x

        Cmd.OfAsync.either f () (Result.mapError Choice2Of2) (Choice2Of2 >> Error)
        |> Cmd.map Msg.AclTypeResponse

    let deleteItem token (item: NavItem) : Cmd<Msg> =
        let deleteErr ex =
            Msg.DeleteResponse(Error(Choice2Of2 ex))

        if item.Type <> NavItemType.Link then
            eprintfn "Deletes only implemented for links"
            Cmd.none
        else
            let f x : Async<Msg> =
                async {
                    let! resp = App.Adapters.Api.Mapped.NavItems.delete token x

                    match resp with
                    | Ok(ni: NavItem) -> return Msg.DeleteResponse(Ok ni)
                    | Error e -> return deleteErr e
                }

            Cmd.OfAsync.either f item.Id id deleteErr


let init token : Model * Cmd<Msg> =
    let next = {
        Token = token
        AclTypes = Array.empty
        Items = None
        CreateParentSelector = None
        Item = None
        Errors = List.empty
    }

    next, Cmd.batch [ Cmd.getRootItems token; Cmd.getAclTypes token ]

let justModel m = m, Cmd.none

let private update msg (model: Model) : Model * Cmd<Msg> =
    printfn "AdminExplorer update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | Msg.AclTypeResponse(Ok v) -> { model with AclTypes = v }, Cmd.none
    | Msg.AclTypeResponse(Error e) -> model |> MLens.addChcError "AclTypeResponse Error" e |> justModel
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
                Item = Some(FolderSelected(NewFolder(NavItem.CreateEmpty None)))
        },
        Cmd.none
    | Msg.DeleteRequested(Choice1Of2 { NavItem = ni })
    | Msg.DeleteRequested(Choice2Of2 ni) -> model, Cmd.deleteItem model.Token ni
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
                Item = Some(FolderSelected(Existing(lni, false)))
        },
        cmd

// module Renders =
//     let renderItemAsList item dispatch selectedItemStore =
// let (|RootErr|RootReady|RootOther|Child| ) navItem (childRemote, selectedItem: SelectedItemType) =
//     let isSelected = selectedItem |> getSelectedItem |> Option.map(fun v -> v.Id = navItem.Id)
//     match childRemote with
type NavItemProps = {
    SelectedItemStore: IStore<SelectedItemType>
    Parent: LazyNavItem option
    Item: Choice<LazyNavItem, NavItem>
    Dispatch: Msg -> unit
}

// types of buttons:
// parent: click to load, click to edit
// child: click to edit
()

let renderNavItem props =
    let activate () =
        // what if it is already active?
        match props.Parent, props.Item with
        | Some parent, Choice2Of2 child ->
            let next = SelectedItemState.ChildSelected(parent, child)

            if props.SelectedItemStore.Value <> Some next then
                props.SelectedItemStore.Update(fun _ ->
                    printfn "Activating child"
                    Some next)
        | _, Choice1Of2 lni ->
            match lni.ChildStore.Value with
            | Responded(Ok _) ->
                props.SelectedItemStore.Update(fun previousSelection ->
                    let nextSelection =
                        match previousSelection with
                        | None -> SelectedItemState.FolderSelected(Existing(lni, false))
                        | Some(FolderSelected(Existing(oldLni, editing))) ->
                            let editing =
                                if oldLni.NavItem.Id = lni.NavItem.Id then
                                    not editing
                                else
                                    false

                            SelectedItemState.FolderSelected(Existing(lni, editing))
                        // is this useful?
                        | Some(FolderSelected(NewFolder lni)) -> FolderSelected(NewFolder lni)
                        | Some x -> x

                    printfn "selected ok: %A" nextSelection
                    nextSelection |> Some)
            | NotRequested -> Msg.PathRequested lni |> props.Dispatch
            | InFlight -> eprintfn "In Flight item click"
            | _ ->
                eprintfn "Selected parent?"
                ()
        | None, Choice2Of2 _ ->
            eprintfn "Orphaned child selected"
            ()

    let navItem =
        match props.Item with
        | Choice1Of2 lni -> lni.NavItem
        | Choice2Of2 ni -> ni

    ()

    let f selectedModel =
        let selected =
            selectedModel
            |> Option.map (function
                | FolderSelected(Existing(lni, _)) -> lni.NavItem
                | FolderSelected(NewFolder ni) -> ni
                | ChildSelected(_, ni) -> ni)

        let isActive =
            selected
            |> Option.map (fun ni -> ni.Id = navItem.Id)
            |> Option.defaultValue false

        bButton "Select Item" [
            match navItem.Icon with
            | ValueString icon -> navItem.Icon |> App.Init.IconSearchType.MuiIcon |> Icons.tryIcon
            | _ -> Html.spanc "icon" []
            Html.span [ text navItem.Name ]
            if isActive then
                Attr.className "is-active"
            onClick (fun _ -> activate ()) []

        ]

    Bind.el (props.SelectedItemStore, f)

()

// let viewLeftNavItem
//     (selectedItemStore: _)
//     dispatch
//     (root: LazyNavItem, childItemOpt: NavItem option)
//     =
//     let navItem = childItemOpt |> Option.defaultValue root.NavItem

//     // navItem |> (fun x -> x.Id) |> printfn "viewLeftNavItem: %A"

//     let simpleNavItemButton name active fActivateOpt children =
//         bButton "Select Item" [
//             text name
//             // will this add button to is-active or overwrite?
//             if active then
//                 Attr.className "is-active"
//             else
//                 match fActivateOpt with
//                 | None -> Attr.disabled true
//                 | Some fActivate -> onClick (fun _ -> fActivate ()) []
//             yield! children
//         ]


//     // stateOpt = None assumes child
//     let navItemButton (item: Choice<_, NavItem>) active disabled stateOpt =
//         let name =
//             match item with
//             | Choice1Of2 lni -> lni.NavItem.Name
//             | Choice2Of2 ni -> ni.Name

//         if disabled then
//             simpleNavItemButton name active None []
//         else
//             let reqData _ = Msg.PathRequested root |> dispatch

//             match stateOpt with
//             | Some InFlight -> simpleNavItemButton name active None [ data_ "InFlight" "true" ]
//             | Some NotRequested -> simpleNavItemButton name active (Some reqData) []
//             | Some(Responded(Error e)) ->
//                 // allow data to be requested again when it fails
//                 simpleNavItemButton name active (Some reqData) [
//                     match e with
//                     | Choice1Of2 e -> data_ "error" <| String.concat "," e
//                     | Choice2Of2 e -> data_ "error" e.Message
//                 ]
//             | None
//             | Some(Responded(Ok _)) ->
//                 // selecting this already-loaded parent
//                 let fSelect _ =
//                     selectedItemStore.Update(fun _ -> Some(SelectedItemState.FolderSelected(item, true)))

//                 simpleNavItemButton name active (Some fSelect) []

//     match childItemOpt with
//     | Some child ->
//         Bind.el (
//             selectedItemStore,
//             fun selectedItem ->
//                 let isActive =
//                     selectedItem
//                     |> getSelectedItem
//                     |> Option.map (fun si -> si.Id = child.Id)
//                     |> Option.defaultValue false

//                 let fSelect _ =
//                     selectedItemStore.Update(fun _ -> Some(SelectedItemState.ChildSelected(root, child)))

//                 simpleNavItemButton child.Name isActive (Some fSelect) []
//         )
//     | None ->

//         // scenarios:
//         // root item, errored children
//         // this is a root item, already selected and already with fetched children
//         // this is a root item, already selected in flight
//         // this is a root item, already selected but shows as not requested ?
//         Bind.el2 root.ChildStore selectedItemStore (fun (childRemote, selectedItem) ->
//             // printfn "Render item child store: %A" root.NavItem.Id

//             let isSelected =
//                 getSelectedItem selectedItem
//                 |> Option.map (fun ni -> ni.Id = navItem.Id)
//                 |> Option.defaultValue false

//             navItemButton (Choice2Of2 navItem) isSelected false (Some childRemote))

let viewLeftNav (items: LazyNavItem[]) (selectedItemStore: IStore<SelectedItemType>) (dispatch: Msg -> unit) =
    printfn "Render viewLeftNav"
    // could this be a navId or a LazyNavItem?
    let store: IStore<LazyNavItem option> = None |> Store.make

    Html.aside [
        Attr.className "menu is-hidden-mobile"
        columns2 [ Attr.className "bordered" ] [
            selectInput
                {
                    Values = items
                    HasEmpty = true
                    SelectType = StoredSelect store
                    ValueGetter =
                        fun lni ->
                            lni.NavItem.Id
                            |> function
                                | NavId x -> x
                    NameGetter = fun lni -> lni.NavItem.Name
                    OptionChildren = fun _ -> List.empty
                }
                [ Html.option [ text "" ] ]
        // Html.select [
        //     Attr.className "select"
        //     Bind.attr ("data-store", store |> Store.map id)
        //     Html.option [ text "" ]
        //     for item in items do
        //         let strId =
        //             match item.NavItem.Id with
        //             | NavId x -> x

        //         Html.option [
        //             // data_ "item" (Core.pretty item.NavItem)
        //             Attr.value strId
        //             text item.NavItem.Name
        //             Attr.title item.NavItem.Description
        //             Handlers.onValueChange ignore (function
        //                 | ValueString rawId -> store.Update(fun _ -> Some rawId)
        //                 | _ -> store.Update(fun _ -> None))
        //             Bind.attr (
        //                 "selected",
        //                 selectedItemStore
        //                 |> Store.map (fun selectedItem ->
        //                     match store.Value, getSelectedItem selectedItem with
        //                     | Some rawId, Some selectedItem -> NavId rawId = selectedItem.Id
        //                     | _ -> false)
        //             )
        //         ]
        // ]

        ] [
            bButton "Add Item" [
                "Add" |> App.Init.IconSearchType.MuiIcon |> Icons.tryIcon
                onClick
                    (fun _ ->
                        // let selectedItem = getSelectedItem selectedItemStore.Value
                        match store.Value with
                        | Some lni -> Msg.StartNewRequested(Some lni) |> dispatch
                        // | Some _ -> eprintfn "addItem selector Value was empty"
                        | None -> Msg.StartNewRequested None |> dispatch

                    )
                    []
            ]
            bButton "Delete Item" [
                "Delete" |> App.Init.IconSearchType.MuiIcon |> Icons.tryIcon
                onClick
                    (fun _ ->

                        match store.Value with
                        | None -> ()
                        | Some lni -> Msg.DeleteRequested(Choice1Of2 lni) |> dispatch)
                    []

            ]
        ]
        yield!
            items
            |> Seq.filter (fun item -> item.NavItem.Type = NavItemType.Folder)
            |> Seq.map (fun lni ->
                renderNavItem {
                    SelectedItemStore = selectedItemStore
                    Dispatch = dispatch
                    Item = Choice1Of2 lni
                    Parent = Some lni
                })
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
                    | Some(FolderSelected(Existing(lni, enabled))) ->
                        Html.li [
                            Html.a [
                                text lni.NavItem.Name
                                onClick
                                    (fun _ ->
                                        selectedItemStore.Update(fun _ ->
                                            Some(FolderSelected(Existing(lni, not enabled)))))
                                    []
                            ]
                        ]
                    | Some(FolderSelected(NewFolder ni)) -> liA (Some "is-active") ni.Name
                    | Some(ChildSelected(lni, ni)) ->
                        Html.li [
                            Html.a [
                                text lni.NavItem.Name
                                onClick
                                    (fun _ ->
                                        selectedItemStore.Update(fun _ -> Some(FolderSelected(Existing(lni, false)))))
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
        | Some(FolderSelected(Existing(lni, editing))) -> $"FolderSelected(lni:%A{lni.NavItem.Name}, %A{editing})"
        | Some(FolderSelected(NewFolder ni)) -> $"FolderSelected(ni:%A{ni.Name})"
        | Some(ChildSelected(lni, ni)) -> $"ChildSelected(lni:%s{lni.NavItem.Name}, ni:%s{ni.Name})"

    let selectedItemStore =
        let getter (model: Model) = model.Item

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
                | FolderSelected(Existing(lni, _)) -> (Some lni, None)
                | FolderSelected(NewFolder _) -> None, None
                | ChildSelected(lni, ni) -> (Some lni, Some ni))
            |> Option.defaultValue (None, None))

    Html.div [
        Attr.className "adminExplorer"
        disposeOnUnmount [ dispose ]
        Samples.viewNavBar ()
        viewBreadCrumbs selectedItemStore
        Html.divc "container" [
            Html.divc "columns" [
                // left selection
                Html.divc "column is-3" [
                    Bind.el (
                        selectedItemStore,
                        function
                        | Some(FolderSelected(Existing(_, false)))
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
                                            | SelectedItemState.FolderSelected(Existing(fs, true)) ->
                                                printfn "Unselecting folder"
                                                FolderSelected(Existing(fs, false)) |> Some
                                            | _ -> None

                                        selectedItemStore.Update(fun _ -> next))
                                    []
                            ]
                    )
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
                // resize if selected
                Bind.el (
                    itemObs,
                    fun (parent, itemOpt) ->
                        let emptyDiv = Html.divc "column is-3" []
                        printfn "Render middle column"

                        match parent with
                        | None -> emptyDiv
                        | Some parent ->
                            let hasItem = Option.isSome itemOpt
                            printfn "Render children"

                            Html.div [
                                Attr.classes [ "column"; if hasItem then " is-3" else " is-9" ]

                                Bind.el (
                                    parent.ChildStore,
                                    function
                                    | InFlight -> Html.div [ text "Loading..." ]
                                    | NotRequested -> Html.divc "is-error" [ text "Not requested" ]
                                    | Responded(Error e) -> Html.divc "is-error" [ text <| string e ]
                                    | Responded(Ok(values, _)) ->
                                        if Array.isEmpty values then
                                            Html.div [ text "No items found." ]
                                        else
                                            Html.ul [
                                                Attr.id "middle-child"
                                                yield!
                                                    values
                                                    |> Seq.map (fun v ->
                                                        Html.li [
                                                            renderNavItem {
                                                                SelectedItemStore = selectedItemStore
                                                                Dispatch = dispatch
                                                                Parent = Some parent
                                                                Item = Choice2Of2 v
                                                            }
                                                        ])

                                            ]

                                )

                            ]

                )
                // begin 3rd column/editor/creator
                let willRenderStore =
                    selectedItemStore
                    |> Store.map (function
                        | None -> false
                        | Some(FolderSelected(Existing(_, false))) -> false
                        | Some _ -> true)

                let flyoverClassAttr =
                    let always = [ "column"; "is-6"; "flyover"; "above-overlay" ]

                    Bind.attr (
                        "class",
                        willRenderStore
                        |> Store.map (
                            function
                            | true -> [ yield! always; "active" ]
                            | false -> always
                            >> String.concat " "
                        )
                    )

                Html.div [
                    flyoverClassAttr
                    let renderEditor item editType =
                        Html.sectionc "hero is-info welcome is-small" [
                            Html.divc "hero-body" [
                                NavUI.view token {
                                    Item = item
                                    EditType = editType
                                    AclTypes = store.Value.AclTypes
                                }

                            ]
                        ]

                    Bind.el (
                        selectedItemStore,
                        fun selectedItem ->
                            match selectedItem with
                            | None
                            | Some(FolderSelected(Existing(_, false))) -> Html.div []
                            | Some(FolderSelected(NewFolder ni)) -> renderEditor ni NavUI.EditType.Parent
                            | Some(FolderSelected(Existing(lni, true))) ->
                                renderEditor lni.NavItem NavUI.EditType.Parent
                            | Some(ChildSelected(lni, ni)) -> renderEditor ni <| NavUI.EditType.Child lni.NavItem
                    )
                ]
            ]
        ]
    ]
    |> Style.withCss
