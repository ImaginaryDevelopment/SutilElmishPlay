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
open App.Adapters.Api.Shared

open App.Components.Gen
open App.Adapters.Api.Mapped
open Core

type SaveResult = SaveType * NavItem

type Model = {
    // NavItem: NavItem

    // a copy of the item from props used for saving
    Item: NavItem
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
            rule ".save-button" [ Css.displayInlineBlock; Css.paddingLeft (px 10) ]
            rule $"#{mainRenderContainer}>h1>span" [ Css.marginRight (px 10) ]
        // rule ".full-overlay" [
        ]

    let withCss = withStyle css

type Msg =
    | SaveRequest
    | SaveResponse of Result<SaveResult, ErrorType>

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


// does this need to fire off resolves for ACLs?
let init item () =
    {
        Item = item
        // Acls = List.empty
        SaveStatus = NotRequested
    // Focus = FocusType.Creator NameFocus
    },
    Cmd.none

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
    EditType: EditType
    AclTypes: AclType[]
    Saved: SaveType * NavItem -> unit
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

    // AclTypes: IReadOnlyStore<AclType seq>
    // consider letting a parent manage this

    toGlobalWindow "NavUI_model" store.Value

    let vStore =
        store
        |> Store.mapRStore
            {
                UseEquality = true
                DebugTitle = None
            }
            (fun model ->
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

    Html.divc "container fill" [
        Attr.id Style.mainRenderContainer
        disposeOnUnmount [ vStore; store ]

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
            text store.Value.Item.Name
            Html.divc "save-button" [

                bButton "Save" [
                    tryIcon (IconSearchType.MuiIcon "Save")
                    // text "Save"
                    onClick (fun _ -> Msg.SaveRequest |> dispatch) List.empty
                ]
            ]
        ]

        formField [ text "Name" ] [
            textInput
                {
                    Titling = "NavUI.Name"
                    Value = store |> Store.map (fun v -> v.Item.Name)
                    OnChange =
                        (fun value ->
                            store.Update(fun old -> {
                                old with
                                    Item = { old.Item with Name = value }
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
                // Attr.isChecked store.Value
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

                App.Components.IconEditor.renderIconEditor { ValueStore = iconValueStore } (function
                    | App.Components.IconEditor.Accepted nextIcon ->
                        store.Update(MLens.updateItem (fun oldItem -> { oldItem with Icon = nextIcon })))

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
                    ResolvedAclStoreOpt = None
                }

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
