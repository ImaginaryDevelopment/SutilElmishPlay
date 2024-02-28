module App.Components.AclEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
open App.Adapters
open App.Adapters.Schema
open App.Adapters.Api
open App.Adapters.Api.Schema
open App.Adapters.Api.Schema.Raw
open App.Adapters.Api.Shared

open App.Adapters.Html
open App.Adapters.Bulma
open App.Adapters.Icons

open Core

module Handlers = App.Adapters.Html.Handlers

// using option to cover the case of ... new but type not selected yet
// should not be able to have IsNew=False; AclValue = None
type AclState = {
    IsNew: bool
    AclValue: (AclType * Set<string>) option
}

type SearchState =
    | AcceptInput
    | Searching

type Model = {
    FocusedAcl: AclState option
    LastAclType: AclName option
    ErrorQueue: (string * System.DateTime) list
    SearchText: string
    SearchState: SearchState
}

type CachedState = {
    AclSearchText: string
    AclType: string option
} with

    member x.AclTypeName =
        x.AclType |> Option.bind (Option.ofValueString >> Option.map AclName)

type AclParamModificationType =
    | AddParam
    | RemoveParam

type AclParentMsg =
    | CreateAcl of AclType * Set<string>
    | Change of AclName * AclParamModificationType * aclParam: string
    | Remove of AclType

type Msg =
    | TypeSelectChange of AclName
    | AclSelect of AclData option
    | AclSearchChange of string
    | AclParamAdd of string
    | AclParamRemove of string

let stateStore: LocalStorage.IAccessor<CachedState> =
    LocalStorage.StorageAccess("AclEditor_CachedState")

let inline justModel m = m, Cmd.none

// assumes we won't ever have an aclRef that can't be found in the acls list
let init
    (aclResolveStore: IStore<ResolvedAclLookup>)
    (aclTypesToItemParamsMap: Map<AclType, Set<string> option>)
    : Model * Cmd<Msg> =
    let cache = stateStore.TryGetValue()

    let searchText =
        cache
        |> Option.bind (fun cs -> cs.AclSearchText |> Option.ofValueString)
        |> Option.defaultValue ""

    let lastAcl =
        cache
        |> Option.bind (fun cs -> cs.AclTypeName)
        |> Option.bind (fun lAclType ->
            aclTypesToItemParamsMap
            |> Map.tryFindKey (fun acl _aclRef -> acl.Name = lAclType))

    let defaultFocus: AclState option =
        lastAcl
        |> Option.orElseWith (fun () -> aclTypesToItemParamsMap.Keys |> Seq.tryHead)
        |> Option.bind (fun k ->
            aclTypesToItemParamsMap
            |> Map.tryFind k
            |> Option.bind (fun v -> v |> Option.map (fun v -> k, v)))
        |> Option.map (fun (k, v) -> { IsNew = false; AclValue = Some(k, v) })

    // let unresolvedAcls =
    //     navItem
    //     |> NavItem.GetRefParams aclTypes
    //     |> Map.choose (fun k (aclType, v) ->
    //         match resolvedAcls |> Map.tryFind k with
    //         | None -> // all params need resolved
    //             Some(k, (aclType, v))
    //         | Some ra ->
    //             let unresolved = ra.Keys |> Set.ofSeq |> Set.difference v
    //             Some(k, (aclType, unresolved)))

    justModel {
        FocusedAcl = defaultFocus
        LastAclType = lastAcl |> Option.map (fun acl -> acl.Name)
        ErrorQueue = List.empty
        SearchText = searchText
        SearchState = AcceptInput
    }

[<RequireQualifiedAccess>]
module private MLens =

    let getFocusedAcl model = model.FocusedAcl
    let getErrors model = model.ErrorQueue

    let addError text model = {
        model with
            ErrorQueue = (text, System.DateTime.Now) :: getErrors model
    }

    /// does not support un-focusing items
    let updateAclRef item f model =
        match model.FocusedAcl |> Option.bind (fun fa -> fa.AclValue) with
        | None -> addError "No Focused Acl" model
        | Some aclR -> {
            model with
                FocusedAcl = Some { item with AclValue = f aclR |> Some }
          }

module SideEffects =

    let saveStateCache (next: Model) =
        Some {
            AclSearchText = next.SearchText
            AclType =
                next.FocusedAcl
                |> Option.bind (fun aclR -> aclR.AclValue)
                |> Option.map (fst >> fun { Name = AclName name } -> name)
        }
        |> stateStore.TrySetValue

let update (aclTypes: AclType seq) (itemAcls: AclData seq) (msg: Msg) (model: Model) =
    printfn "AclEditor: '%A'" msg

    let justError title = MLens.addError title model, Cmd.none

    let findAclTypeOr (AclName aclName) f =
        aclTypes
        |> Seq.tryFind (fun aclType -> aclType.Name = AclName aclName)
        |> function
            | None -> justError $"Could not find AclType: '%s{aclName}'"
            | Some aclType -> f aclType


    match msg, model with
    // blocks

    | AclSearchChange _, { SearchState = Searching } -> justError "Server is busy"
    | AclParamAdd _, { FocusedAcl = None } -> justError "No Acl selected"
    | AclParamRemove _, { FocusedAcl = None } -> justError "No Acl selected"
    // | AclSearchRequest, {SearchState = Searching, _} -> justError "A Search is already in flight"

    // questionable usefulness
    | TypeSelectChange _, { FocusedAcl = None } -> justError "Start a new acl first"
    | TypeSelectChange _, { FocusedAcl = Some { IsNew = false } } -> justError "Cannot change type on existing acl"
    | TypeSelectChange v, _ when itemAcls |> Seq.exists (fun acl -> acl.Name = v) ->
        justError "Cannot change to existing acl type"

    // actions

    | Msg.AclSearchChange text, { SearchState = AcceptInput } -> justModel { model with SearchText = text }
    | Msg.AclParamAdd value, { FocusedAcl = Some fa } ->
        model
        |> MLens.updateAclRef fa (fun (aclType, ar) -> aclType, ar |> Set.add value)
        |> justModel
    | Msg.AclParamRemove value, { FocusedAcl = Some fa } ->
        model
        |> MLens.updateAclRef fa (fun (aclType, ar) -> aclType, ar |> Set.remove value)
        |> justModel
    | TypeSelectChange aclName, { FocusedAcl = Some fa } ->
        printfn "TypeSelected change"

        // lens doesn't like there not being an AclValue
        findAclTypeOr aclName (fun aclType ->
            match fa.AclValue with
            | None ->
                justModel {
                    model with
                        FocusedAcl =
                            Some {
                                fa with
                                    AclValue = Some(aclType, Set.empty)
                            }
                }
            | Some _ -> model |> MLens.updateAclRef fa (fun _ -> aclType, Set.empty) |> justModel)

    | AclSelect(Some aclData), _ ->
        printfn "AclSelected"

        findAclTypeOr aclData.Name (fun aclType ->
            {
                model with
                    FocusedAcl =
                        Some {
                            IsNew = false
                            AclValue = Some(aclType, aclData.Parameters)
                        }
            }
            |> justModel)

    | AclSelect None, _ ->
        printfn "new AclSelected"

        justModel {
            model with
                FocusedAcl = Some { IsNew = true; AclValue = None }
        }
    |> fun (next, cmd) ->
        let next =
            match SideEffects.saveStateCache next with
            | Ok() -> next
            | Error e -> MLens.addError e model

        printfn "Focused will be: %A" next.FocusedAcl
        next, cmd

module Renderers =
    type AclTypeSelectorProps = {
        Focus: AclState option
        AclTypes: AclType seq
        ExistingAclTypes: AclName list
        SelectedType: AclName option
    }

    let renderAclTypeSelector (props: AclTypeSelectorProps) dispatch =
        // log aclTypes
        let disabled =
            match props.Focus with
            | None -> true
            | Some f -> not f.IsNew

        Html.select [
            text "Acl!"
            Attr.className "select"
            Attr.disabled disabled
            Handlers.onValueChangeIf dispatch (fun v ->
                Option.ofValueString v |> Option.map (AclName >> Msg.TypeSelectChange))
            Html.option [ text "" ]
            for ({ Name = AclName oName } as o) in
                props.AclTypes
                |> Seq.filter (fun { Name = aclName } ->
                    props.SelectedType = Some aclName
                    || props.ExistingAclTypes |> List.exists (fun eat -> eat = aclName) |> not) do
                Html.option [
                    Attr.value oName
                    text oName
                    Attr.title <| pretty o
                    if props.SelectedType = Some o.Name then
                        Attr.selected true
                ]
        ]

    let renderAclSearchResults aclParams (aclType: AclType) onParamAdd =
        let ps =
            match aclParams with
            | [] ->
                match aclType.AclParamType with
                | AclParameterType.Selectable values -> values |> Seq.map (fun p -> p, p) |> List.ofSeq
                | _ -> List.empty
            | values -> values

        Html.divc "box" [
            for (value, name) in ps do
                Html.divc "columns" [
                    Html.divc "column is-one-fifth" [
                        bButton "Add Acl Param" [
                            tryIcon (IconSearchType.MuiIcon "Add")

                            // onClick (fun _ -> AclParentMsg.Change(aclType.Name, AddParam, value) |> dispatchParent) []
                            onClick (fun _ -> onParamAdd (name, value)) []
                        ]
                    ]
                    Html.divc "column" [ text name; Attr.title value ]

                // Html.ul [
                //     for (value, name) in ps do
                //         Html.li [ text name; Attr.title value ]
                // ]
                ]
        ]

    type RenderAclProps = {
        SearchState: SearchState
        SearchText: string
        // holds options that can be selected, or search results
        SelectableAclParams: (string * string) list
        ResolvedAclLookup: ResolvedAclLookup
        AclType: AclType
        Item: AclState
    }

    let renderAclParams props dispatch dispatchParent =
        let isNotNone =
            match props.AclType.AclParamType with
            | AclParameterType.None -> false
            | _ -> true

        let selectedParams = props.Item.AclValue |> Option.map snd

        Html.div [
            data_ "file" "AclEditor"
            data_ "method" "renderAclParams"
            Html.divc "columns" [
                Html.divc "column is-two-fifths" [
                    if isNotNone then
                        Html.divc "box" [
                            // needs border or underline or font weight work
                            Html.h2 [ text "Existing" ]
                            Html.ul [
                                for p in selectedParams |> Option.defaultValue Set.empty do
                                    Html.li [
                                        Html.divc "columns" [
                                            Html.divc "column is-one-fifth" [
                                                bButton "Remove Acl Param" [
                                                    tryIcon (IconSearchType.MuiIcon "Remove")
                                                    onClick (fun _ -> Msg.AclParamRemove p |> dispatch) List.empty

                                                ]
                                            ]
                                            Html.divc "column" [
                                                Html.pre [
                                                    match
                                                        props.ResolvedAclLookup
                                                        |> Map.tryFind props.AclType.Name
                                                        |> Option.bind (Map.tryFind (AclRefId p))
                                                    with
                                                    | None ->
                                                        text p
                                                        Attr.title p
                                                    | Some {
                                                               Reference = AclRefId r
                                                               DisplayName = display
                                                           } ->
                                                        Attr.title r
                                                        text display

                                                ]

                                            ]

                                        ]
                                    ]
                            ]
                        ]
                ]
                Html.divc "column" [
                    let isSearchable =
                        match props.AclType.AclParamType with
                        | AclParameterType.Reference true -> true
                        | _ -> false

                    if isSearchable then

                        Html.inputc "text" [
                            Attr.value props.SearchText

                            match props.SearchState with
                            | AcceptInput -> autofocus
                            | SearchState.Searching -> Attr.disabled true

                            onInput (fun e -> e.inputElement.value |> Msg.AclSearchChange |> dispatch) []
                        ]

                        bButton "Search" [
                            // text "Search"
                            tryIcon (IconSearchType.MuiIcon "Search")
                            onClick (fun _ -> ()) []
                        ]
                ]
                Html.divc "column is-three-fifths" [
                    data_ "purpose" "params"
                    renderAclSearchResults props.SelectableAclParams props.AclType (fun (pName, pValue) ->
                        if props.Item.IsNew then
                            Msg.AclParamAdd pValue |> dispatch
                        else
                            AclParentMsg.Change(props.AclType.Name, AclParamModificationType.AddParam, pValue)
                            |> dispatchParent)
                ]
            ]
        ]

    type AclProps = {
        AclData: AclData
        AclType: AclType
        IsSelected: bool
        DispatchParent: Dispatch<AclParentMsg>
    }

    // render a display of the acl for a table-like thing, not an editor
    let renderAcl (props: AclProps) dispatch =
        if props.AclType.Name <> props.AclData.Name then
            failwith $"renderAcl {props.AclType.Name} - {props.AclData.Name}"

        let isActiveRow = props.IsSelected

        let isConfigurable =
            match props.AclType.AclParamType with
            | AclParameterType.None -> false
            | _ -> true

        let tButton title isActiveButton isPrimary isDisabled icon fOnClick =
            let bClasses =
                seq [
                    "is-small"
                    if isPrimary then
                        "is-primary"
                    if isActiveButton then "is-light" else "is-link"

                ]
                |> Choice2Of2
                |> Static

            bButtonC title bClasses [
                tryIcon icon
                Attr.disabled isDisabled
                if not isDisabled then
                    onClick (fun _ -> fOnClick ()) List.empty
            ]

        Html.divc "columns" [
            data_ "file" "AclEditor"
            data_ "method" "renderAcl"
            Html.divc "column is-one-fifth buttonColumn" [
                if isConfigurable then
                    tButton "Edit Acl" isActiveRow true isActiveRow (IconSearchType.MuiIcon "Edit") (fun _ ->
                        props.AclData |> Some |> Msg.AclSelect |> dispatch)
            ]
            Html.divc "column is-one-fifth buttonColumn" [
                tButton "Delete" isActiveRow false false (MuiIcon "Delete") (fun _ ->
                    AclParentMsg.Remove props.AclType |> props.DispatchParent)
            ]
            Html.divc "column is-four-fifths" [
                Attr.classes [
                    if isActiveRow then
                        "has-text-weight-bold"
                ]
                text (AclName.getText props.AclData.Name)
                Html.spanc "info" [ text "*"; Attr.title (Core.pretty props.AclData) ]
                Html.spanc "info" [ text "*"; Attr.title (Core.pretty props.AclType) ]
            ]
            Html.divc "column is-one-fifth" [
                if props.AclData.Parameters.Count < 1 && isConfigurable then
                    Html.divc "is-warning" [ Attr.title "Not configured"; tryIcon (MuiIcon "Error") ]
                elif isConfigurable then
                    text (string props.AclData.Parameters.Count)
                else
                    text ""
            ]
        ]

open Sutil.Styling
open type Feliz.length
open type Feliz.borderStyle

let css = [
    // rule "span.info" Gen.CssRules.titleIndicator
    rule "h2" [ Css.borderBottom (px 1, solid, "black") ]

    // not working
    // rule ".panel-block>*" [
    //     Css.custom("!width", "100%")
    // ]

    rule "div.buttonColumn" [
        Css.height (em 1.0)
        Css.width (em 2.5)
        Css.flexShrink 0
        Css.marginRight (px 5)
    ]

    rule ".columns" [ Css.marginTop (px 0); Css.marginBottom (px 0); Css.marginLeft (px 5) ]
]

type ResolvedAcl = { Id: string; Display: string }

type AclEditorProps = {
    ItemAcls: AclData list
    AclTypes: AclType seq
    DispatchParent: Dispatch<AclParentMsg>
    // in case a parent wants to manage this store
    AclLookupStore: IStore<ResolvedAclLookup> option
}

// allow them to edit or create one
let renderAclsEditor (props: AclEditorProps) =
    // if Core.windowData then
    //     printfn "AclEditor Render"

    toGlobalWindow "aclEditor_props" props

    let aclResolveStore =
        props.AclLookupStore |> Option.defaultWith (fun () -> Map.empty |> Store.make)

    let store, dispatch =
        // not the model
        // map of all acl types to current item's parameters if there are any
        let aclTypesToItemParamsMap: Map<AclType, Set<string> option> =
            props.AclTypes
            |> Seq.map (fun aclType ->
                aclType,
                props.ItemAcls
                |> Seq.tryFind (fun ar -> ar.Name = aclType.Name)
                |> Option.map (fun ar -> ar.Parameters))
            |> Map.ofSeq

        aclTypesToItemParamsMap
        |> Store.makeElmish (init aclResolveStore) (update props.AclTypes props.ItemAcls) ignore


    let updateModel () =
        toGlobalWindow "aclEditor_model" store.Value |> ignore

    updateModel ()
    // TODO: check store for unresolved params, and fire off acl type change to parent or other msg to get them looked up

    let renderFocusedAcl (focusedAcl: AclState) = [
        // Html.spanc "info" [ text "*"; Attr.title (Core.pretty focusedAcl) ]
        let display = {|
            Name = focusedAcl.AclValue |> Option.map (fst >> fun aclValue -> aclValue.Name)
            IsNew = focusedAcl.IsNew
            Params = focusedAcl.AclValue |> Option.map (snd >> Set.toArray)
        |}

        Html.spanc "info" [ text "*"; Attr.title (Core.pretty display) ]

        Html.divc "box" [
            // let renderAclParams (idMap:Map<string,AclRefState>) (aclType: Acl) (item: AclRef) =
            match focusedAcl.AclValue with
            | Some(selectedAclType, _) ->

                Bind.el2 App.Global.resolvedAclLookup App.Global.aclSearchResponse (fun (m, aSR) ->
                    // are we in a selectable or searchable?
                    let aclParams =
                        match selectedAclType.AclParamType with
                        | AclParameterType.Reference true ->
                            aSR
                            |> Map.tryFind selectedAclType.Name
                            |> Option.map (fun (_searchText, aclDisplays) ->
                                aclDisplays
                                |> Seq.map
                                    (fun
                                        {
                                            Reference = AclRefId r
                                            DisplayName = display
                                        } -> r, display))

                        | AclParameterType.Selectable selectableParameters ->
                            selectableParameters |> Seq.map Tuple2.replicate |> Some
                        | _ -> None
                        |> Option.map List.ofSeq
                        |> Option.defaultValue List.empty

                    printfn
                        "Rendering Acl Params with %i aclNames and %i items"
                        m.Count
                        (m |> Map.toSeq |> Seq.collect (snd >> Map.toSeq) |> Seq.length)

                    updateModel ()

                    Renderers.renderAclParams
                        {
                            SearchText = store.Value.SearchText
                            SearchState = store.Value.SearchState
                            Item = focusedAcl
                            SelectableAclParams = aclParams
                            ResolvedAclLookup = m
                            AclType = selectedAclType
                        }
                        dispatch
                        props.DispatchParent)
            | None -> ()
        ]
    ]

    printfn "Render AclEditor"

    Html.divc "fill" [
        disposeOnUnmount [ store ]
        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay
        data_ "file" "AclEditor"
        data_ "method" "renderAclsEditor"

        Bind.el (
            store |> Store.map MLens.getFocusedAcl,
            fun focusAclOpt ->
                updateModel ()

                let selectedAclName =
                    focusAclOpt
                    |> Option.bind (fun acl -> acl.AclValue)
                    |> Option.map (fst >> fun item -> item.Name)

                Html.div [
                    Renderers.renderAclTypeSelector
                        {
                            AclTypes = props.AclTypes
                            Focus = focusAclOpt
                            SelectedType = selectedAclName
                            ExistingAclTypes = props.ItemAcls |> Seq.map (fun iar -> iar.Name) |> List.ofSeq
                        }
                        dispatch
                    match focusAclOpt with
                    | Some {
                               IsNew = true
                               AclValue = Some(aclType, pSet)
                           } ->
                        let addText = "Add New Acl"

                        bButton addText [
                            data_ "button-purpose" addText
                            // floppy disk icon
                            tryIcon (IconSearchType.MuiIcon "Save")
                            // text addText
                            onClick (fun _ -> AclParentMsg.CreateAcl(aclType, pSet) |> props.DispatchParent) []
                        ]

                        bButton "Cancel Add Acl" [
                            tryIcon (IconSearchType.MuiIcon "Cancel")

                        ]
                    | _ ->
                        bButton "Add Acl" [
                            data_ "button-purpose" "Add Acl"
                            tryIcon (IconSearchType.MuiIcon "Add")
                            // text "Add Acl"
                            onClick (fun _ -> Msg.AclSelect None |> dispatch) []
                        ]

                    match focusAclOpt with
                    | Some focusedAcl -> yield! renderFocusedAcl focusedAcl
                    | None -> ()
                ]
        )
        Html.h2 [ text "Existing Acls" ]

        Bind.el (
            store |> Store.map MLens.getFocusedAcl,
            fun focusOpt ->

                Html.div [
                    match props.ItemAcls with
                    | [] -> Html.divc "has-text-warning-light" [ text "No Acls present" ]
                    | _ ->
                        for (i, aclData) in props.ItemAcls |> Seq.indexed do
                            let thisAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = aclData.Name)

                            Html.div [
                                if i % 2 = 0 then
                                    Attr.className "has-background-link-light"
                                // render the selected Acl if there is one
                                match thisAclType with
                                | None -> Html.div [ text <| AclName.getText aclData.Name ]
                                | Some aclType ->
                                    Renderers.renderAcl
                                        {
                                            AclType = aclType
                                            AclData = aclData
                                            DispatchParent = props.DispatchParent
                                            IsSelected =
                                                focusOpt
                                                |> Option.bind (fun focus -> focus.AclValue)
                                                |> Option.map fst
                                                |> Option.map (fun selectedAclType ->
                                                    selectedAclType.Name = aclType.Name)
                                                |> Option.defaultValue false

                                        }
                                        dispatch
                            ]
                ]
        )
    ]
    |> withStyle css
