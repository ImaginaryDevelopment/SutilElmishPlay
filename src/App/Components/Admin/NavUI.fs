module App.Components.Admin.NavUI

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
            rule $".help.is-danger" [ Css.fontSize (em 1.5); Css.backgroundColor "white" ]
        // rule ".full-overlay" [
        ]

    let withCss = withStyle css

type Msg =
    | SaveRequest
    | SaveResponse of Result<SaveResult, ErrorType>
    | SearchAdminResponse of Result<ApiAclSearchResponse, ErrorType>
    | BulkAdminResponse of Result<AdminPickerBulkResolveResponse, ErrorType>
    | SearchRequest of string
    | ManagerSelection of AclRefId


module Commands =
    let upsert token (vni: ValidNavItem) : Cmd<Msg> =
        let f () =
            async {
                let st =
                    if NavItem.IsNew vni.ValidNavItem then
                        SaveType.Create
                    else
                        SaveType.Update

                let! resp =
                    if st = SaveType.Create then
                        App.Adapters.Api.Mapped.NavItems.create token vni
                    else
                        App.Adapters.Api.Mapped.NavItems.save token vni

                match resp with
                | Ok v -> return Ok(st, v)
                | Error e -> return Error e
            }

        Cmd.OfAsync.either f () id (fun ex -> Choice2Of2 ex |> Error)
        |> Cmd.map Msg.SaveResponse

    let searchAdmin token text =
        let f = App.Adapters.Api.Shared.searchAdmin token

        Cmd.OfAsync.either f text id (Choice2Of2 >> Error)
        |> Cmd.map Msg.SearchAdminResponse

    let bulkResolveAdmin token navId =
        let f = App.Adapters.Api.Shared.getFolderAdmins token

        Cmd.OfAsync.either f navId id (Choice2Of2 >> Error)
        |> Cmd.map Msg.BulkAdminResponse

let init token item () =
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
            Commands.bulkResolveAdmin token item.Id
        else
            Cmd.none

    model, cmd

module MLens =
    let updateItem f model = { model with Item = f model.Item }

    let updateManager aclRefId model =
        model
        |> updateItem (fun x -> {
            x with
                Managers = x.Managers |> Set.toggle aclRefId
        })

    let onManagerResolves (aclTypes: AclType seq) (aclDisplays: AclDisplay seq) =
        aclDisplays
        |> Seq.choose (fun v ->
            if v.Type = AclReferenceType.User then
                aclTypes
                |> Seq.tryFind (fun aclType -> aclType.Name |> App.Global.isGroup)
                |> Option.map (fun aclType -> aclType, v)
            elif v.Type = AclReferenceType.Group then
                aclTypes
                |> Seq.tryFind (fun aclType -> aclType.Name |> App.Global.isGroup)
                |> Option.map (fun aclType -> aclType, v)
            else
                Core.log v
                eprintfn "Unrecognized search result: %A" v
                None)
        |> Map.ofSeqGroups
        |> Map.iter (fun aclType values -> App.Global.ResolvedAclLookup.addValues aclType.Name values)

let private update token (aclTypes: AclType seq) (onSave: SaveResult -> unit) msg (model: Model) : Model * Cmd<Msg> =
    printfn "NavUI update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | SaveRequest ->
        // this is only working for updates, and is NOT ensuring a valid item before sending the update
        let vResult = ValidNavItem.ValidateNavItem model.Item

        match vResult with
        | Ok vni -> { model with SaveStatus = InFlight }, Commands.upsert token vni
        // assumes the error was already added to the model
        | Error e ->
            eprintfn "Save is not valid"

            e
            |> Map.iter (fun k errors ->
                match k with
                | None ->
                    use _ = Core.logGroup (Some "Save General Errors")
                    errors |> List.iter Core.log
                | Some fieldName ->
                    use _ = Core.logGroup (Some(string fieldName))
                    errors |> List.iter Core.log)

            let errorsForDisplay =
                e
                |> Map.toSeq
                |> Seq.collect (fun (k, errors) ->
                    match k with
                    | Some(ValueString fieldName) -> errors |> Seq.map (fun error -> $"{fieldName}-{error}")
                    | _ -> errors)
                |> Array.ofSeq

            {
                model with
                    SaveStatus = Responded(Error(Choice1Of2 errorsForDisplay))
            },
            Cmd.none

    | SaveResponse(Error e) ->
        match e with
        | Choice1Of2 x -> eprintfn "Failed save: %A" x
        | Choice2Of2 x -> eprintfn "Failed save2: %A" x

        {
            model with
                SaveStatus = RemoteData.Responded(Error e)
        },
        Cmd.none

    | SaveResponse(Ok sr) ->
        onSave sr
        model, Cmd.none
    | SearchRequest text -> model, Commands.searchAdmin token text
    | ManagerSelection aclRefId -> model |> MLens.updateManager aclRefId, Cmd.none
    | BulkAdminResponse(Ok v) ->
        v.Resolved |> MLens.onManagerResolves aclTypes

        model, Cmd.none
    | SearchAdminResponse(Ok v) ->
        // we have to match up displays with their acl types
        // since this is not a pure result and can contain multiples
        v.Results |> MLens.onManagerResolves aclTypes

        {
            model with
                SearchIsInFlight = false
                ManagerSearchResults = v.Results |> List.ofArray
        },
        Cmd.none


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

    let renderManagerEditor (aclTypes: AclType[]) (store: IStore<Model>) (dispatch: Msg -> unit) =
        let mgrSearchResultsRStore =
            store |> Store.mapRStore (fun model -> model.ManagerSearchResults)

        let sStore = "" |> Store.make

        let mStore: IReadOnlyStore<Set<AclRefId>> =
            store |> Store.mapRStore (fun v -> v.Item.Managers)

        let rapRStore: IReadOnlyStore<Map<AclRefId, AclDisplay>> =
            let aclTypes =
                aclTypes
                |> Array.filter (fun v -> App.Global.isGroup v.Name || App.Global.isUser v.Name)

            App.Global.makeTypesUnsafeRStore aclTypes

        collapsibleCard (Some(Choice2Of2 true)) (text "Managers") [
            CardContentType.Content [
                AclTypeEditor.Renderers.renderReferenceParams {
                    OnSearch = Msg.SearchRequest >> dispatch
                    OnClick = Msg.ManagerSelection >> dispatch
                    SearchStore = sStore // IStore<string>
                    ResolveStore = rapRStore
                    SearchResults = mgrSearchResultsRStore //  IReadOnlyStore<AclDisplay list>
                    SearchInFlight = store |> Store.map (fun v -> v.SearchIsInFlight) // System.IObservable<bool>
                    CurrentParamsStore = mStore
                }
            ]
        ]
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
    | { EditType = Parent; Item = ni } when NavItem.IsNew ni -> CreateRootFolder ni
    | { EditType = Parent; Item = ni } -> EditFolder ni
    | { EditType = Child pni; Item = ni } when NavItem.IsNew ni -> CreateChild(pni, ni)
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
        ()
        |> Store.makeElmish (init token item) (update token props.AclTypes props.Saved) ignore

    let unsubscribe = store.Subscribe(fun v -> App.Global.selectedItem <- Some v.Item)

    // AclTypes: IReadOnlyStore<AclType seq>
    // consider letting a parent manage this

    toGlobalWindow "NavUI_model" store.Value

    let vStore =
        store
        |> Store.mapRStore (fun model ->
            let vResult = ValidNavItem.ValidateNavItem model.Item
            vResult |> Option.ofResult)
    // merge sources model remote data and navItem validation errors
    let errorStore =
        Store.zip
            (vStore |> Store.map snd)
            (store
             |> Store.map (fun m -> m.SaveStatus)
             |> Observable.map (function
                 | Responded(Ok v) -> None
                 | Responded(Error e) -> Some e
                 | _ -> None))


    let renderErrors nameOpt = [

        Bind.el (
            errorStore,
            fun (validationErrors, apiResponseErrors) ->
                match nameOpt with
                | ValueString name -> fragment <| RenderHelpers.getError validationErrors (Some name)
                | _ ->
                    fragment [
                        match apiResponseErrors with
                        | None -> ()
                        | Some(Choice1Of2 errors) -> yield! RenderHelpers.renderErrors "Save Errors" errors
                        | Some(Choice2Of2 ex) -> yield! RenderHelpers.renderErrors "Save Error" [ ex.Message ]
                        yield! RenderHelpers.getError (snd vStore.Value) None

                    ]
        )
    ]

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

        Html.divc "is-danger" [ yield! renderErrors null ]

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
