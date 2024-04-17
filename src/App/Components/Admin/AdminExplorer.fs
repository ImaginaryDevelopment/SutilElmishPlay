module App.Components.Admin.Explorer

// https://github.com/BulmaTemplates/bulma-templates/blob/master/templates/admin.html
// demo https://bulmatemplates.github.io/bulma-templates/templates/admin.html

open BReusable

open Sutil
open Sutil.Core
open Sutil.CoreElements

open App.Schema
open App.Adapters.Icons
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

type ItemsState = Result<LazyNavItem[], AdminErrorType>

type Model = {
    Token: string
    Items: ItemsState option
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

    let private zOverlay = 31

    let globalRules = [
        rule ".above-overlay" [ Css.zIndex <| zOverlay + 3 ]

        rule ".flyover" [
            Css.positionFixed
            Css.overflowAuto
            Css.top 0
            // start off screen
            Css.right (percent -25)
            // adjust as needed
            Css.width (percent 50)
            Css.height (vh 100)
            Css.transition ("right 0.4s ease, opacity 0.1s ease")
            // kills the animation, but stops it from being on screen when not in use
            Css.opacity 0

            // Css.animationDuration (System.TimeSpan.FromMilliseconds 700)
            // Css.animationTimingFunctionEaseIn
            Css.zIndex <| zOverlay + 5
            Css.backgroundColor "#3298dc"
            Css.paddingLeft (px 10)
        ]

        rule ".flyover.active" [ Css.right 0; Css.opacity 1 ]

    ]

    // let specialCss =
    //     let mutable iHaveSpoken = false

    //     fun () ->
    //         if not iHaveSpoken then
    //             iHaveSpoken <- true

    //             // hacky work around for attr class binding overwriting component-level styling

    //             Sutil.Styling.addGlobalStyleSheet Browser.Dom.document specialRules ()


    // https://github.com/BulmaTemplates/bulma-templates/blob/master/css/admin.css
    let css =
        // navbar is ~30

        [
            rule "*" [ Css.lineHeight (rem 1.5); Css.height (percent 100); Css.marginLeft 0 ]
            rule ".adminExplorer" [ Css.backgroundColor "#ECF0F3" ]
            rule "nav.navbar" [ Css.borderTop (px 4., solid, "#276cda"); Css.marginBottom (rem 1.0) ]
            rule ".navbar-item.brand-text" [ Css.fontWeight 300 ]
            rule ".navbar-item, .navbar-link" [ Css.fontSize (px 14); Css.fontWeight 700 ]

            rule "#admin-explorer-left-nav" [
                Css.displayFlex
                Css.flexDirectionColumn
                Css.borderRadius (px 4)
                Css.backgroundColor "rgb(238, 238, 238)"
            // Css.backgroundColor "rgb(255, 255, 255)"
            // Css.color "rgba(0, 0, 0, 0.87)"
            // Css.flex "1 0 auto"
            // Css.marginTop (px 30)
            // Css.positionFixed
            // Css.margin 0
            // Css.marginTop (px 59)
            ]

            rule "#admin-explorer-left-nav>button" [
                Css.border (px 0, Feliz.borderStyle.none, "black")
                Css.marginBottom (px 10)
                Css.padding (px 15)
                Css.width (percent 100)
                Css.color "rgb(61, 60, 65)"
            ]
            rule "#admin-explorer-left-nav>button:hover" [
                Css.backgroundColor "rgb(255, 255, 255)"

            ]

            rule "#admin-explorer-left-nav>button.is-active" [
                Css.backgroundColor "rgba(0, 108, 185, 0.12)"
                Css.color "rgb(0, 108, 185)"
            ]

            rule ".columns" [ Css.width (percent 100); Css.height (percent 100); Css.marginLeft 0 ]
            // linksComponent-grid
            rule "#middle-child" [
                Css.displayGrid
                Css.width (percent 100)
                Css.gridTemplateColumns [ fr 1; fr 1; fr 1; fr 1 ]
                // from body
                yield! [
                    Css.fontWeight 400
                    Css.fontSize (rem 1)
                    // built-in won't allow 1.5
                    Css.custom ("line-height", "1.5")
                    Css.custom ("letter-spacing", "0.00938em")
                ]

            ]
            // MuiPaper-root MuiPaper-outlined MuiPaper-rounded MuiCard-root LinksComponent-button css-______
            rule "#middle-child>div" [
                Css.margin (px 5)
                Css.overflowHidden
            // Css.width (percent 100)


            ]
            // MuiButtonBase-root MuiCardActionArea-root LinksComponent-buttonAction css-1m5f78l
            rule "#middle-child>div>button" [
                Css.padding (px 20)
                Css.custom ("white-space", "normal")
                Css.height (px 75)
                Css.overflowAuto

                // dynamically added
                yield! [
                    Css.alignItemsCenter
                    Css.justifyContentCenter
                    Css.positionRelative
                    Css.boxSizingBorderBox
                    Css.outlineStyleNone
                    Css.borderStyleNone
                    Css.margin 0
                    Css.verticalAlignMiddle
                    Css.textDecorationNone
                    Css.width (percent 100)
                ]
            // Css.displayGrid
            // Css.gridTemplateColumns [ fr 1; fr 3 ]

            ]

            // not working properly or the target isn't rising properly
            rule ".overlay" [
                Css.positionFixed
                // Css.displayBlock
                Css.width (percent 100)
                Css.height (percent 100)
                Css.top 0
                Css.left 0
                Css.right 0
                Css.bottom 0
                Css.backgroundColor "rgba(0,0,0,0.5)"
                Css.zIndex zOverlay
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
        ]

    let withCss x =
        // Sutil.Styling.addGlobalStyleSheet Browser.Dom.document css ()
        // specialCss ()
        withStyle css x

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

    let updateItems nextItems model = {
        model with
            Items = Some(Ok nextItems)
    }

    let getEditingItem =
        function
        | FolderSelected(Existing(folder, true)) -> Some(Choice1Of2 folder)
        | FolderSelected(NewFolder lni) -> Some(Choice2Of2 lni)
        | ChildSelected(_, child) -> Some(Choice2Of2 child)
        | _ -> None


    let unselectItemState oldState =
        match oldState with
        | FolderSelected(NewFolder _) -> None
        | FolderSelected(Existing(folder, true)) -> Some(FolderSelected(Existing(folder, false)))
        | FolderSelected(Existing(_, false)) -> None
        | ChildSelected(parent, _) -> (parent, false) |> Existing |> FolderSelected |> Some

    let unselectItem model =
        match model.Item with
        | None -> model
        | Some v -> {
            model with
                Item = unselectItemState v
          }

    let hasChildId childId (lni: LazyNavItem) =
        lni.ChildStore.Value
        |> RemoteData.TryGet
        |> Option.map (fst >> Array.exists (fun ni -> ni.Id = childId))
        |> Option.defaultValue false


    let updateChildStore (childStore: IStore<RemoteData<NavItem[] * bool>>) (nextItems, x) =
        childStore.Update(fun _ -> Responded(Ok(nextItems, x)))

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
    | Saved of SaveType * NavItem

// allow for pathRequest, and direct store selection update
// let getActivateItemAttrs dispatch

module Cmd =
    let getRootItems token : Cmd<Msg> =
        let f x =
            async {
                let! resp = App.Adapters.Api.Mapped.NavItems.getNavRoot token x

                match resp with
                | Ok v -> return Msg.RootResponse(Ok v)
                | Error e -> return Msg.RootResponse(Error e)
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
                    | Error e -> return Msg.PathResponse(lni, Error e)
                }

            Cmd.OfAsync.either f lni.NavItem.Path id (fun ex -> Msg.PathResponse(lni, Error(Choice2Of2 ex)))

        | InFlight -> Cmd.none

    let getAclTypes token : Cmd<Msg> =
        let f x =
            App.Adapters.Api.Mapped.getAclTypes token x

        Cmd.OfAsync.either f () id (Choice2Of2 >> Error) |> Cmd.map Msg.AclTypeResponse

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
                    return Msg.DeleteResponse resp
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

let (|CreateFolder|CreateItem|UpdateFolder|UpdateItem|InvalidSave|): NavItem * SaveType * ItemsState option -> _ =
    function
    | { Type = Folder }, SaveType.Create, Some(Ok items) -> CreateFolder items

    | { Type = Link; Parent = parentPath }, SaveType.Create, Some(Ok items) ->
        // find parent by... item.Parent?
        items
        |> Array.tryFindIndex (fun lni -> String.equalsI lni.NavItem.Path parentPath)
        |> function
            | Some i -> CreateItem(i, items)
            | None -> InvalidSave "Could not find parent"

    | { Type = Folder; Id = navId }, SaveType.Update, Some(Ok items) ->
        items
        |> Seq.tryFindIndex (fun lni -> lni.NavItem.Id = navId)
        |> function
            | Some i -> UpdateFolder(i, items)
            | None -> InvalidSave "Could not find target item to update"

    | { Type = Link; Id = navId }, SaveType.Update, Some(Ok items) ->
        // ignores possibility there is a duplicate id in two parents
        items
        |> Seq.choose (fun lni ->
            lni.ChildStore.Value
            |> function
                | RemoteData.Responded(Ok(children, expanded)) ->
                    children
                    |> Array.tryFindIndex (fun child -> child.Id = navId)
                    |> Option.map (fun i -> lni.ChildStore, (children, expanded), i)
                | _ -> None)
        |> Seq.tryHead
        |> function
            | None -> InvalidSave "Could not find target child to update"
            | Some(childStore, childThing, i) -> UpdateItem(childStore, childThing, i)

    | _, _, Some(Error _) -> InvalidSave "Batman Error: Parents invalid"
    | _, _, None -> InvalidSave "Batman Error: No Parents found"


let private update msg (model: Model) : Model * Cmd<Msg> =
    printfn "AdminExplorer update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | Msg.AclTypeResponse(Ok v) -> { model with AclTypes = v }, Cmd.none
    | Msg.AclTypeResponse(Error e) -> model |> MLens.addChcError "AclTypeResponse Error" e |> justModel
    | Msg.DeleteResponse(Error e) ->
        eprintfn "Failed to delete: %A" e
        model, Cmd.none

    | Msg.DeleteResponse(Ok v) ->
        // reload root?
        model, Cmd.getRootItems model.Token

    | Msg.Saved(st: SaveType, nextItem) ->
        // assume folders are root, and links are children
        // what if this is a successful create?
        printfn "Save complete, attempting explorer update"

        match nextItem, st, model.Items with
        | UpdateFolder(x, items) ->
            printfn "Folder saved, updating"

            items
            |> Array.updateI x (fun item ->
                printfn "Updating folder by index: %s" <| NavItem.GetName item.NavItem
                { item with NavItem = nextItem })
            |> function
                | Ok nextItems ->
                    printfn "Update kinda done?"
                    MLens.updateItems nextItems model |> MLens.unselectItem, Cmd.none
                | Error e ->
                    printfn "Update kinda error"
                    model |> MLens.addError e, Cmd.none

        | CreateFolder items ->
            let childStore = Store.make NotRequested

            let nextItems =
                Array.append items [|
                    {
                        NavItem = nextItem
                        ChildStore = childStore
                    }
                |]

            MLens.updateItems nextItems model |> MLens.unselectItem, Cmd.none

        // oops this is bad code, it should be creating something in a parent store
        | CreateItem(x, items) ->
            items
            |> Array.updateI x (fun oldItem -> { oldItem with NavItem = nextItem })
            |> function
                | Ok nextItems -> MLens.updateItems nextItems model |> MLens.unselectItem, Cmd.none
                | Error e -> model |> MLens.addError e, Cmd.none

        | UpdateItem(childStore, (children, expanded), i) ->
            printfn "AdminExplorer updateItem: %i(%i) %A-%s" i children.Length nextItem.Id nextItem.Icon
            // update children, use them to update parent store directly (no model change needed)
            children
            |> Array.updateI i (fun oldValue ->
                printfn "UpdateItem: %i from %A-%s to %A-%s" i oldValue.Id oldValue.Icon nextItem.Id nextItem.Icon
                nextItem)
            |> function
                | Ok nextItems ->
                    // this isn't updating the explorer somehow, try updating the model below
                    MLens.updateChildStore childStore (nextItems, expanded)

                    // {model with ItemAcls = model.ItemAcls |> Array.updateI }
                    model |> MLens.unselectItem, Cmd.none
                | Error e ->
                    eprintfn "Error updating item in explorer: %A" e
                    model |> MLens.addError e, Cmd.none

        | InvalidSave e -> model |> MLens.addError e, Cmd.none

    | Msg.StartNewRequested(Some parent) ->
        // (LazyNavItem * NavItem option) option
        printfn "Starting new child under '%s'" <| NavItem.GetName parent.NavItem

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
    | Msg.DeleteRequested(Choice2Of2 ni) ->
        // TODO: this won't delete folders currently
        MLens.unselectItem model, Cmd.deleteItem model.Token ni
    | Msg.RootResponse(Ok v) ->

        // TODO: filter on user is imp or is in managers list? or is that on the backend
        let nextItems =
            v
            // |> Array.filter (fun navItem -> isNull navItem.Managers || props.user)
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
                | FolderSelected(Existing(lni, _)) -> Some lni, lni.NavItem
                | FolderSelected(NewFolder ni) -> None, ni
                | ChildSelected(lni, ni) -> Some lni, ni)

        // not working
        // include the case where the parent is-active because a child is selected
        let isActive =
            selected
            |> Option.map (fun (lniOpt, ni) ->
                ni.Id = navItem.Id
                || lniOpt |> Option.map (MLens.hasChildId navItem.Id) |> Option.defaultValue false)
            |> Option.defaultValue false


        bButton "Select Item" [
            match navItem.Icon with
            | ValueString icon -> navItem.Icon |> IconSearchType.MuiIcon |> tryIcon
            | _ -> Html.spanc "icon" []
            Html.span [ text <| NavItem.GetName navItem ]
            match isActive, navItem.Enabled with
            | false, false -> Attr.classes [ "has-background-grey-lighter" ]
            | false, true -> ()
            | true, true -> Attr.classes [ "is-active" ]
            | true, false -> Attr.classes [ "is-active"; "has-background-grey-lighter" ]

            onClick (fun _ -> activate ()) []

        ]

    Bind.el (props.SelectedItemStore, f)

()

let viewLeftNav (items: LazyNavItem[]) (selectedItemStore: IStore<SelectedItemType>) (dispatch: Msg -> unit) =
    printfn "Render viewLeftNav"
    // could this be a navId or a LazyNavItem?
    let store: IStore<LazyNavItem option> =
        selectedItemStore
        |> Store.mapStore "leftNavSelect" true {
            Getter =
                Option.bind (function
                    | FolderSelected(Existing(lni, _)) -> Some lni
                    | _ -> None)
            Setter =
                (fun next (oldParent, oldChild) ->
                    match next, oldParent with
                    // clearing all selection if they clear the select
                    | None, _ -> None
                    | Some lni, _ -> FolderSelected(Existing(lni, true)) |> Some)
        }

    Html.aside [
        Attr.id "admin-explorer-left-nav"
        Attr.className "menu is-hidden-mobile"
        Html.div [
            formFieldAddons [] [
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
                        NameGetter = fun lni -> NavItem.GetName lni.NavItem
                        OptionChildren = fun _ -> List.empty
                    }
                    []
                bButton "Add Item" [
                    "Add" |> IconSearchType.MuiIcon |> tryIcon
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

            ] []

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
                                text <| NavItem.GetName lni.NavItem
                                onClick
                                    (fun _ ->
                                        selectedItemStore.Update(fun _ ->
                                            Some(FolderSelected(Existing(lni, not enabled)))))
                                    []
                            ]
                        ]
                    | Some(FolderSelected(NewFolder ni)) -> liA (Some "is-active") <| NavItem.GetName ni
                    | Some(ChildSelected(lni, ni)) ->
                        Html.li [
                            Html.a [
                                text <| NavItem.GetName lni.NavItem
                                onClick
                                    (fun _ ->
                                        selectedItemStore.Update(fun _ -> Some(FolderSelected(Existing(lni, false)))))
                                    []
                            ]
                        ]

                        liA None <| NavItem.GetName ni
                ]
        )
    ]

let view token (ai: App.Adapters.Msal.AuthenticationResult) =
    printfn "AdminExplorer"
    Core.log ai
    let store, dispatch = token |> Store.makeElmish init update ignore
    toGlobalWindow "adminExplorer_model" store.Value

    let getValueDisplay =
        function
        | None -> "null"
        | Some(FolderSelected(Existing(lni, editing))) ->
            $"FolderSelected(lni:%A{NavItem.GetName lni.NavItem}, %A{editing})"
        | Some(FolderSelected(NewFolder ni)) -> $"FolderSelected(ni:%A{NavItem.GetName ni})"
        | Some(ChildSelected(lni, ni)) ->
            $"ChildSelected(lni:%s{NavItem.GetName lni.NavItem}, ni:%s{NavItem.GetName ni})"

    let selectedItemStore =
        let getter (model: Model) = model.Item

        let setter nextValue (model: Model, oldValue) =

            printfn "Setting selected item: %A -> %A" (getValueDisplay oldValue) (getValueDisplay nextValue)
            { model with Item = nextValue }

        store
        |> Store.mapStore "AESelectedItem" true { Getter = getter; Setter = setter }

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
                | FolderSelected(Existing(lni, _)) -> (Some lni, Some lni.NavItem)
                | FolderSelected(NewFolder _) -> None, None
                | ChildSelected(lni, ni) -> (Some lni, Some ni))
            |> Option.defaultValue (None, None))

    Html.div [
        Attr.className "adminExplorer"
        disposeOnUnmount [ dispose ]
        // Samples.viewNavBar ()
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
                            // printfn "Overlay render hide"
                            Html.div [ Attr.id "overlay"; Attr.styleAppend [ Css.visibilityHidden ] ]
                        // TODO: add left side props editor display
                        | Some selectedItem ->
                            Html.divc "overlay" [
                                Attr.id "overlay"
                                // leave folder selected but turn off editing if editing is on
                                onClick
                                    (fun _ ->
                                        let activate () =
                                            let next = MLens.unselectItemState selectedItem

                                            selectedItemStore.Update(fun _ -> next)

                                        let confirmActivate () =
                                            if Core.promptConfirm "Abandon changes?" then
                                                activate ()

                                        App.Global.selectedItem
                                        |> function
                                            | None ->
                                                printfn "No Selected item found"
                                                activate ()
                                            | Some ni ->
                                                if ni.Hash <> NavItem.CalcHash ni then
                                                    confirmActivate ()
                                                else
                                                    printfn "No Changes found"
                                                    activate ())

                                    []
                            ]
                    )
                    Bind.el (
                        store |> Store.map (fun model -> model.Items),
                        function
                        | None -> Html.div [ text "Loading..." ]
                        | Some(Ok items) ->

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
                        let hasItem = Option.isSome itemOpt
                        printfn "Render middle column(hasItem:%A)" hasItem

                        match parent with
                        | None -> emptyDiv
                        | Some parent ->

                            Html.div [
                                Attr.classes [ "column"; if hasItem then " is-9" else " is-9" ]

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
                                            Html.div [
                                                Attr.id "middle-child"
                                                yield!
                                                    values
                                                    |> Seq.map (fun v ->
                                                        Html.div [
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
                    let always = [ "flyover"; "above-overlay" ]

                    let obs =
                        willRenderStore
                        |> Store.map (function
                            | true -> [
                                //"column"; "is-6";
                                yield! always
                                "active"
                              ]
                            | false -> always)

                    Bind.classNames obs


                Html.div [
                    // Attr.className "column is-3"
                    flyoverClassAttr
                    // lniOpt should NOT be item's parent, but the item container
                    let renderEditor (item: Choice<LazyNavItem, NavItem>) editType =
                        let barred = "/Root/Blended Learning"

                        let navItem =
                            match item with
                            | Choice1Of2 lni -> lni.NavItem
                            | Choice2Of2 ni -> ni

                        printfn "Item.Parent: %s" navItem.Parent

                        Html.sectionc "hero is-info welcome is-small" [
                            Html.divc "hero-body" [
                                NavUI.view token {
                                    Item = navItem
                                    EditType = editType
                                    AclTypes = store.Value.AclTypes
                                    // TODO: set this based on current user
                                    UserCanManage = true
                                    // if it has an icon already allow editing, otherwise allow icons if they aren't an area we don't want icons
                                    AllowIcon =
                                        navItem.Icon |> String.isValueString
                                        || (String.equalsI navItem.Parent barred |> not)
                                    Saved = (fun nextItem -> Msg.Saved nextItem |> dispatch)
                                    Delete =
                                        (fun () ->
                                            // empty item, just deselect
                                            if NavItem.IsNew navItem then
                                                selectedItemStore.Update(fun _ -> None)
                                            // can't delete a new item
                                            else
                                                item |> Msg.DeleteRequested |> dispatch)
                                }
                            ]
                        ]

                    Bind.el (
                        selectedItemStore,
                        fun selectedItem ->
                            match selectedItem with
                            | None
                            | Some(FolderSelected(Existing(_, false))) -> Html.div []
                            | Some(FolderSelected(NewFolder ni)) -> renderEditor (Choice2Of2 ni) NavUI.EditType.Parent
                            | Some(FolderSelected(Existing(lni, true))) ->
                                renderEditor (Choice1Of2 lni) NavUI.EditType.Parent
                            | Some(ChildSelected(lni, ni)) ->
                                renderEditor (Choice2Of2 ni) <| NavUI.EditType.Child lni.NavItem
                    )
                ]
            ]
        ]
    ]
    |> Style.withCss
