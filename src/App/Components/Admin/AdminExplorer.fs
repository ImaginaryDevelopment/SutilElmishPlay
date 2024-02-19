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

    let css = [
        // rule "span.info" Gen.CssRules.titleIndicator
        rule ".leftMe" [
            // Css.borderBottom (px 1, solid, "black")
            Css.marginLeft (px -100)
        ]
    ]

    let withCss = withStyle css

type LazyNavItem = {
    NavItem: NavItem
    ChildStore: IStore<RemoteData<NavItem[] * bool>>
}

type Model = {
    Token: string
    Items: Result<LazyNavItem[], string * System.DateTime> option
    Item: NavItem option
    Errors: (string * System.DateTime) list
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
        match lni.ChildStore.Value with
        | Responded _
        | NotRequested ->
            let f x =
                async {
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
    printfn "AdminExplorer update: %A" msg

    match msg with
    | Msg.RootResponse(Ok v) ->
        let childStore = Store.make NotRequested

        let nextItems =
            v
            |> Array.map (fun navItem -> {
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
    // | Msg.PathToggle(lni, expanded) ->

    //     model, Cmd.none


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

let viewLeftNavItem (selectedItem: NavItem) dispatch (rootItem: LazyNavItem) =
    let isActive = rootItem.NavItem.Id = selectedItem.Id

    let genRootItem children =
        Html.pc "menu-label" [
            text rootItem.NavItem.Name
            if isActive then
                Attr.className "is-active"
            yield! children
        ]

    Bind.el (
        rootItem.ChildStore,
        fun childRemote ->
            printfn "Rerender item child store"

            match childRemote with
            // TODO: plus to try to expand children
            | NotRequested ->
                fragment [
                    genRootItem [
                        bButton "Expand Item" [
                            Icons.tryIcon (App.Init.IconSearchType.MuiIcon "Add")
                            // onClick (fun _ -> AclParentMsg.Change(aclType.Name, AddParam, value) |> dispatchParent) []
                            onClick (fun _ -> Msg.PathRequested rootItem |> dispatch) []
                        ]

                    ]
                ]
            // TODO: loading indicator for children
            | InFlight ->
                fragment [
                    genRootItem [
                    // TODO: loading indicator
                    ]
                ]

            | Responded(Ok(items, expanded)) ->
                fragment [
                    genRootItem [
                        bButton "Collapse Item" [
                            Icons.tryIcon (App.Init.IconSearchType.MuiIcon "Minimize")
                            onClick (fun _ -> rootItem.ChildStore.Update(fun _ -> Responded(Ok(items, false)))) []
                        ]
                    ]
                    if expanded then
                        Html.ul [
                            Attr.className "menu-list"
                            yield!
                                items
                                |> Seq.map (fun item ->
                                    Html.li [
                                        Html.a [
                                            text item.Name

                                        ]
                                    ])
                        ]
                    else
                        bButton "Expand Item" [
                            Icons.tryIcon (App.Init.IconSearchType.MuiIcon "Add")
                            // onClick (fun _ -> AclParentMsg.Change(aclType.Name, AddParam, value) |> dispatchParent) []
                            onClick (fun _ -> rootItem.ChildStore.Update(fun _ -> Responded(Ok(items, true)))) []
                        ]
                ]

    )

let viewLeftNav (items: LazyNavItem[]) (selectedItem: NavItem) dispatch =
    fragment [
        Html.aside [
            Attr.className "menu is-hidden-mobile"
            yield!
                items
                |> Seq.filter (fun item -> item.NavItem.Type = NavItemType.Folder)
                |> Seq.map (viewLeftNavItem selectedItem dispatch)
        ]
    ]

let view token =
    printfn "AdminExplorer"
    let store, dispatch = token |> Store.makeElmish init update ignore

    Html.div [
        // Attr.className "leftMe"
        Samples.viewNavBar ()
        Html.divc "container" [
            Html.divc "columns" [
                Html.divc "column is-3" [
                    Bind.el (
                        store |> Store.map (fun model -> model.Items),
                        function
                        | None -> Html.div [ text "Loading..." ]
                        | Some(Ok items) ->
                            let selectedItem =
                                store.Value.Item |> Option.defaultWith (fun () -> items[0].NavItem)

                            viewLeftNav items selectedItem dispatch
                        | Some(Error e) ->
                            Html.div [
                                Attr.className "is-error"
                                App.Components.Gen.ErrorHandling.renderErrors [ e ]
                            ]
                    )
                ]
                Html.divc "column is-9" [
                    Samples.viewBreadCrumbs ()
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
    |> Style.withCss
