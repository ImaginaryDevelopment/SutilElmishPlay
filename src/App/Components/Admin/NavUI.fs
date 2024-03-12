module App.Components.Admin.NavUI

open BReusable

open Sutil
open Sutil.Core
open Sutil.CoreElements

open App.Adapters.Schema
open App.Adapters.Icons
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api.Schema

open App.Components.Gen
open App.Adapters.Api.Mapped
open Core

type SaveResult = SaveType * NavItem

type Model = {
    // NavItem: NavItem

    ManagerSearchResults: AclDisplay list
    // a copy of the item from props used for saving
    Item: NavItem
    SearchIsInFlight: bool
    // Acls: (AclData * AclType) list
    SaveStatus: RemoteData<SaveResult>
}

module Style =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle

    let mainRenderContainer = "nav-ui-editor"

    // https://github.com/BulmaTemplates/bulma-templates/blob/master/css/admin.css
    let css =

        [
            rule ".fill" [ Css.width (percent 100); Css.height (percent 100) ]
            rule ".card" [ Css.marginBottom (px 10) ]
            // rule ".save-button" [ Css.displayInlineBlock; Css.paddingLeft (px 10) ]
            rule ".field.has-addons" [ Css.displayInlineFlex ]
            rule $"#{mainRenderContainer}>h1>span" [ Css.marginRight (px 10) ]
        // rule ".full-overlay" [
        ]

    let withCss = withStyle css

type Msg =
    | SaveRequest
    | SaveResponse of Result<SaveResult, ErrorType>
    | SearchAdminResponse of Result<AclSearchResult, ErrorType>
    | SearchRequest of string
    | ManagerSelection of AclRefId


module Commands =

    let save token navItem : Cmd<Msg> =
        let f x =
            async {
                let! resp = App.Adapters.Api.Mapped.NavItems.save token x

                match resp with
                | Ok v ->
                    let st =
                        if navItem.Id = NavId "" then
                            SaveType.Create
                        else
                            SaveType.Update

                    return Ok(st, v)
                | Error e ->
                    let x: ErrorType = Choice2Of2 e
                    return Error x
            }

        Cmd.OfAsync.either f navItem id (fun ex -> Choice2Of2 ex |> Error)
        |> Cmd.map Msg.SaveResponse

    let searchAdmin token text =
        let f = App.Adapters.Api.Shared.searchAdmin token

        Cmd.OfAsync.either f text id Error
        |> Cmd.map (Result.mapError Choice2Of2 >> Msg.SearchAdminResponse)



// does this need to fire off resolves for ACLs?
let init item () =
    let model = {
        Item = item
        SearchIsInFlight = false
        // Acls = List.empty
        SaveStatus = NotRequested
        ManagerSearchResults = List.empty
    // Focus = FocusType.Creator NameFocus
    }

    let cmd =
        if Set.isEmpty item.Managers |> not then
            // TODO: fire off a resolve
            Cmd.none
        else
            Cmd.none

    model, cmd

module MLens =
    let updateItem f model = { model with Item = f model.Item }


let private update token (onSave: SaveResult -> unit) msg (model: Model) : Model * Cmd<Msg> =
    printfn "NavUI update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | SaveRequest -> { model with SaveStatus = InFlight }, Commands.save token model.Item
    | SaveResponse(Error e) ->
        eprintfn "Failed save: %A" e
        model, Cmd.none
    | SaveResponse(Ok sr) ->
        onSave sr
        model, Cmd.none
    | SearchRequest text -> model, Commands.searchAdmin token text


module RenderHelpers =

    let renderErrors title errors = [
        Attr.title title
        Attr.classes [ "help"; "is-danger" ]
        text (String.concat "," errors)
    ]

    let renderErrorMapMaybe (vErrors: Map<'t option, _> option) fToString (x: 't option) =
        vErrors
        |> Option.bind (fun eMap -> eMap |> Map.tryFind x)
        |> Option.defaultValue List.empty
        |> function
            | [] -> []
            | errors -> renderErrors (fToString x) errors

    let getError vErrors =
        renderErrorMapMaybe vErrors (function
            | None -> ""
            | Some v -> v)

    let renderManagerEditor (aclTypes: AclType[]) store (dispatch: Msg -> unit) =
        let mgrSearchResultsRStore =
            store |> Store.mapRStore (fun model -> model.ManagerSearchResults)

        let sStore = "" |> Store.make

        let mStore: IReadOnlyStore<Set<AclRefId>> =
            store |> Store.mapRStore (fun v -> v.Item.Managers)

        let rapRStore: IReadOnlyStore<Map<AclRefId, AclDisplay>> =
            let aclTypes =
                aclTypes
                |> Array.filter (fun v -> v.Name = AclName "Allow-By-Group" || v.Name = AclName "Allow-By-User")

            App.Global.makeTypesUnsafeRStore aclTypes

        AclTypeEditor.Renderers.renderReferenceParams {
            OnSearch = Msg.SearchRequest >> dispatch
            OnClick = Msg.ManagerSelection >> dispatch
            SearchStore = sStore // IStore<string>
            ResolveStore = rapRStore
            SearchResults = mgrSearchResultsRStore //  IReadOnlyStore<AclDisplay list>
            SearchInFlight = store |> Store.map (fun v -> v.SearchIsInFlight) // System.IObservable<bool>
            CurrentParamsStore = mStore
        }
        // collapsibleCard (Some(Choice2Of2 true)) (text "Managers") [

        //     CardContentType.Content [
        //         Html.divc "fill" [
        //             columns2 [] [
        //                 Html.h2 [ text "Has" ]
        //                 // selected items
        //                 Bind.el (
        //                     mStore,
        //                     fun p ->

        //                         // Set.count p |> printfn "Render Has with: %i"

        //                         Html.ul [
        //                             // TODO: this needs to be sorted by observable results
        //                             for aclRefId in p do
        //                                 Html.li [
        //                                     AclTypeEditor.renderAclParams
        //                                         (p, rapRStore)
        //                                         (Choice1Of2 aclRefId)
        //                                         onAclClick
        //                                 ]
        //                         ]
        //                 )

        //             ] [
        //                 Html.h2 [ text "Available" ]
        //                 // HACK: this is looking at all search results, not just current search results
        //                 // unselected items - based on the lookup of this acl name, or based on the search results? uh oh
        //                 Bind.el2 mStore mgrSearchResultsRStore (fun (currentParams, searchResults) ->
        //                     let cp = currentParams
        //                     let lookupMap = rapRStore

        //                     Html.ul [
        //                         for ad in
        //                             searchResults |> List.filter (fun ad -> cp |> Set.contains ad.Reference |> not) do
        //                             Html.li [
        //                                 AclTypeEditor.renderAclParams
        //                                     (currentParams, lookupMap)
        //                                     (Choice2Of2 ad)
        //                                     onAclClick
        //                             ]
        //                     ])

        //             ]


        //         ]
        //     ]
        // ]
        |> fun x -> x

// None,None => Create new root folder
// Some _, None -> Create new child - Link
// None, Some => Edit folder
// Some _, Some _ -> Edit child - Link
type EditType =
    | Parent
    | Child of NavItem

// should we be keeping resolved ACLs past the lifetime of this item editor?
type NavUIProps = {
    Item: NavItem
    AllowIcon: bool
    EditType: EditType
    AclTypes: AclType[]
    Saved: SaveType * NavItem -> unit
    Delete: unit -> unit
    UserCanManage: bool
}

let (|CreateRootFolder|CreateChild|EditFolder|EditChild|) = // InvalidAttempt|) =
    function
    | {
          EditType = Parent
          Item = { Id = NavId "" } as ni
      } -> CreateRootFolder ni
    | { EditType = Parent; Item = ni } -> EditFolder ni
    | {
          EditType = Child pni
          Item = { Id = NavId "" } as ni
      } -> CreateChild(pni, ni)
    | { EditType = Child pni; Item = ni } -> EditChild(pni, ni)

let view token (props: NavUIProps) =
    toGlobalWindow "NavUI_props" props
    printfn "NavUI render"

    let item, isCreate, hasParent =
        match props with
        | CreateRootFolder ni -> ni, true, false
        | EditFolder ni -> ni, false, false
        | CreateChild(_parent, ni) -> { ni with Type = Link }, true, true
        | EditChild(_parent, ni) -> { ni with Type = Link }, false, true

    printfn "NavUI: type: %A, isCreate:%b, hasParent:%b" item.Type isCreate hasParent

    let store, dispatch =
        () |> Store.makeElmish (init item) (update token props.Saved) ignore

    let unsubscribe = store.Subscribe(fun v -> App.Global.selectedItem <- Some v.Item)

    // AclTypes: IReadOnlyStore<AclType seq>
    // consider letting a parent manage this

    toGlobalWindow "NavUI_model" store.Value

    let vStore =
        store
        |> Store.mapRStore (fun model ->
            let vResult = ValidNavItem.ValidateNavItem model.Item
            vResult |> Option.ofResult)

    let renderErrors nameOpt =
        match nameOpt with
        | ValueString name -> [

            Bind.el (
                vStore |> Store.map (fun (_, vErrors) -> vErrors),
                fun v -> fragment <| RenderHelpers.getError v (Some name)
            )
          ]
        | _ ->
            let elems = RenderHelpers.getError (snd vStore.Value) None
            elems

    let getItemTitling (model: Model) =
        match NavItem.GetName model.Item with
        | ValueString name ->
            if model.Item.Type = NavItemType.Folder then
                name
            else
                $"%s{model.Item.Parent}/{name}"
        | _ ->
            if model.Item.Type = NavItemType.Folder then
                "New folder name"
            else
                $"%s{model.Item.Parent}: new link"

    Html.divc "container fill" [
        Attr.id Style.mainRenderContainer
        disposeOnUnmount [ unsubscribe; vStore; store ]
        let itemTitling = store |> Store.map getItemTitling

        Html.h1 [
            Attr.className "title"

            Bind.el (
                store |> Store.map (fun model -> model.Item.Type),
                fun itemType ->
                    if itemType = NavItemType.Folder then
                        "FolderOpen"
                    else
                        "Link"
                    |> IconSearchType.MuiIcon
                    |> tryIcon
            )
            Bind.el (itemTitling, (fun t -> Html.span [ text t ]))
            formFieldAddons [] [

                Html.spanc "save-button" [

                    bButton "Save" [
                        tryIcon (IconSearchType.MuiIcon "Save")
                        // text "Save"
                        onClick (fun _ -> Msg.SaveRequest |> dispatch) List.empty
                    ]
                ]
                Html.spanc "delete-button" [

                    bButton "Delete Item" [
                        "Delete" |> IconSearchType.MuiIcon |> tryIcon
                        onClick (fun _ -> props.Delete()) []

                    ]
                ]
            ] []
        ]

        formField [ text "Name" ] [
            textInput
                {
                    Titling = getItemTitling store.Value
                    Value = store |> Store.map (fun v -> NavItem.GetName v.Item)
                    OnChange =
                        (fun value ->
                            store.Update(fun old -> {
                                old with
                                    Item = old.Item |> NavItem.SetName value
                            }))
                    DebounceOverride = None
                }
                []

        ]
        <| renderErrors "Name"

        let enabledTitle = nameof item.Enabled
        let urlTitle = nameof item.Url

        formField [ text enabledTitle ] [
            let eStore = store |> Store.map (fun v -> v.Item.Enabled)

            Html.inputc "checkbox" [
                type' "checkbox"
                Attr.name enabledTitle
                Bind.attr ("checked", value = eStore)
                Attr.title enabledTitle
                Handlers.onValueChange ignore (fun _ ->
                    store.Update(fun model -> {
                        model with
                            Item = {
                                model.Item with
                                    Enabled = not model.Item.Enabled
                            }
                    }))
            ]
        ] []

        if store.Value.Item.Type = NavItemType.Link then
            formField [ text urlTitle ] [
                textInput
                    {
                        Titling = $"NavUI.{urlTitle}"
                        Value = store |> Store.map (fun v -> v.Item.Url)
                        OnChange =
                            fun value -> store.Update(MLens.updateItem (fun oldItem -> { oldItem with Url = value }))
                        DebounceOverride = None
                    }
                    []
            ]
            <| renderErrors urlTitle

        let weightTitle = nameof item.Weight

        formField [ text weightTitle ] [
            textInput
                {
                    Titling = $"NavUI.{weightTitle}"
                    Value = store |> Store.map (fun v -> string v.Item.Weight)
                    OnChange =
                        (fun value ->
                            store.Update(MLens.updateItem (fun oldItem -> { oldItem with Weight = int value })))
                    DebounceOverride = None
                }
                []

        ] []

        if props.AllowIcon then
            card [
                CardContentType.Header(text "Icon")
                CardContentType.Content [
                    let iconValueStore =
                        store
                        |> Store.mapStore "iconValueStore" true {
                            Getter = fun v -> v.Item.Icon
                            Setter =
                                fun iconValue (model, _) -> {
                                    model with
                                        Item = { model.Item with Icon = iconValue }
                                }
                        }

                    App.Components.IconEditor.renderIconEditor { ValueStore = iconValueStore }

                ]
            ]

        card [
            CardContentType.Header(text "Acl Explorer")
            CardContentType.Content [
                let iaStore =
                    store
                    |> Store.mapStore "iaStore" true {
                        Getter = (fun model -> model.Item.AclRefs |> Map.map (fun _ -> Set.map AclRefId))
                        Setter =
                            (fun ar (model, _) -> {
                                model with
                                    Item = {
                                        model.Item with
                                            AclRefs = ar |> Map.map (fun _ -> Set.map AclRefId.getText)
                                    }
                            })
                    }

                App.Components.Admin.AclEditor.render {
                    Token = token
                    AclTypes = props.AclTypes // AclType seq
                    ItemAcls = iaStore
                    NavId = item.Id
                    ResolvedAclStoreOpt = None
                }
            ]
            if props.UserCanManage then
                CardContentType.Content [

                    let element = RenderHelpers.renderManagerEditor props.AclTypes store dispatch
                    element
                ]

        ]

        // TODO: maybe enable LS flag to make this visible to user instead of always
        collapsibleCard None (text "Raw") [
            CardContentType.Content [
                Bind.el (store |> Store.map (fun v -> v.Item), (fun item -> Html.pre [ text <| Core.pretty item ]))
            ]
        ]
    ]
    |> Style.withCss
