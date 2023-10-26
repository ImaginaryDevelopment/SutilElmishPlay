module App.Components.AclEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
open App.Adapters
open App.Adapters.Api
open App.Adapters.Html
open App.Adapters.Bulma
open Core
open Gen.Icons

module Handlers = App.Adapters.Html.Handlers

// TODO: is this complete and a proper representation? move to api perhaps
// type AclType =
//     | Selectable of values: string[] option * multiValue: bool
//     | Reference of multiValue: bool

type AclState = { IsNew: bool; AclRef: AclRef }

type SearchState =
    | AcceptInput
    | Searching

type Model = {
    FocusedAcl: AclState option
    ErrorQueue: (string * System.DateTime) list
    SearchText: string
    SearchState: SearchState
}

type CachedState = { AclSearchText: string }

type AclParentMsg =
    // which Acl Type is highlighted
    // None is a valid option
    | TypeChange of Acl
    | SearchRequest of AclRefValueArgs
    | Create of AclRef * Acl
    | Remove of Acl

type Msg =
    | TypeSelectChange of string
    | AclSelect of AclRef option
    | AclSearchChange of string


type AclRefState =
    | Requested
    | Response of Result<AclDisplay, Gen.ErrorType>

let stateStore: LocalStorage.IAccessor<CachedState> =
    LocalStorage.StorageAccess("AclEditor_CachedState")

let emptyAcl = { Name = ""; Parameters = Array.empty }
let inline justModel m = m, Cmd.none

let init aclStateRefOpt : Model * Cmd<Msg> =
    let searchText =
        stateStore.TryGetValue()
        |> Option.bind (fun cs -> cs.AclSearchText |> Option.ofValueString)
        |> Option.defaultValue ""

    justModel {
        FocusedAcl = aclStateRefOpt
        ErrorQueue = List.empty
        SearchText = searchText
        SearchState = AcceptInput
    }

[<RequireQualifiedAccess>]
module private MLens =
    let updateAclRef item f model = {
        model with
            FocusedAcl = Some { item with AclRef = f item.AclRef }
    }

    let getFocusedAcl model = model.FocusedAcl
    let getErrors model = model.ErrorQueue

    let addError text model = {
        model with
            ErrorQueue = (text, System.DateTime.Now) :: getErrors model
    }

module SideEffects =

    let saveStateCache (next: Model) =
        Some { AclSearchText = next.SearchText } |> stateStore.TrySetValue

let update (itemAcls: AclRef seq) (msg: Msg) (model: Model) =
    printfn "AclEditor: '%A'" msg
    let justError title = MLens.addError title model, Cmd.none

    match msg, model with
    // blocks

    | AclSearchChange _, { SearchState = Searching } -> justError "Server is busy"
    // | AclSearchRequest, {SearchState = Searching, _} -> justError "A Search is already in flight"
    | TypeSelectChange _, { FocusedAcl = None } -> justError "Start a new acl first"
    | TypeSelectChange _, { FocusedAcl = Some { IsNew = false } } -> justError "Cannot change type on existing acl"
    | TypeSelectChange v, _ when itemAcls |> Seq.exists (fun acl -> acl.Name = v) ->
        justError "Cannot change to existing acl type"

    // actions

    | Msg.AclSearchChange text, { SearchState = AcceptInput } -> justModel { model with SearchText = text }
    | TypeSelectChange v, { FocusedAcl = Some x } ->
        printfn "TypeSelected change"
        model |> MLens.updateAclRef x (fun x -> { x with Name = v }) |> justModel

    // TODO: should we warn if they have unsaved changed and select another acl?
    | AclSelect(Some v), _ ->
        printfn "AclSelected"

        justModel {
            model with
                FocusedAcl = Some { IsNew = false; AclRef = v }
        }
    | AclSelect None, _ ->
        printfn "new AclSelected"

        justModel {
            model with
                FocusedAcl =
                    Some {
                        IsNew = true
                        AclRef = { Name = ""; Parameters = Array.empty }
                    }
        }
    |> fun (next, cmd) ->
        let next =
            match SideEffects.saveStateCache next with
            | Ok() -> next
            | Error e -> MLens.addError e model

        next, cmd

module Renderers =
    type AclTypeSelectorProps = {
        Focus: AclState option
        AclTypes: Acl seq
        SelectedType: string
    }

    type AclProps = {
        AclRef: AclRef
        Acl: Acl
        IsSelected: bool
        DispatchParent: Dispatch<AclParentMsg>
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
            Handlers.onValueChangeIf dispatch (fun v -> Option.ofValueString v |> Option.map Msg.TypeSelectChange)
            Html.option [ text "" ]
            for o in props.AclTypes do
                Html.option [
                    Attr.value o.Name
                    text o.Name
                    if props.SelectedType = o.Name then
                        Attr.selected true
                ]
        ]

    let renderAclSearchResults aclParams (aclType: Acl) =
        let ps =
            match aclParams with
            | [] ->
                aclType.SelectableParameters
                |> Option.defaultValue Array.empty
                |> Seq.map (fun p -> p, p)
                |> List.ofSeq
            | values -> values

        Html.divc "box" [
            for (value, name) in ps do
                Html.divc "columns" [
                    Html.divc "column" [ bButton "Add" [ tryIcon (App.Init.IconSearchType.MuiIcon "Add") ] ]
                    Html.divc "column" [ text name; Attr.title value ]

                // Html.ul [
                //     for (value, name) in ps do
                //         Html.li [ text name; Attr.title value ]
                // ]
                ]
        ]

    let renderAclParams
        searchState
        (aclParams: (string * string) list)
        (idMap: Map<string, AclRefState>)
        (aclType: Acl)
        (item: AclRef)
        dispatch
        dispatchParent
        =
        Html.div [
            data_ "file" "AclEditor"
            data_ "method" "renderAclParams"
            Html.divc "columns" [
                Html.divc "column" [
                    if item.Parameters.Length > 0 || aclType.ParameterType <> AclParameterType.None then
                        Html.divc "box" [
                            // needs border or underline or font weight work
                            Html.h2 [ text "Existing" ]
                            Html.ul [
                                for p in item.Parameters do
                                    Html.li [
                                        match idMap |> Map.tryFind p with
                                        | Some AclRefState.Requested
                                        | None ->
                                            text p
                                            Attr.title p
                                        | Some(AclRefState.Response(Ok x)) ->
                                            Attr.title x.Reference
                                            text x.DisplayName
                                        | Some(AclRefState.Response(Error e)) ->
                                            Attr.title (string e)
                                            text p
                                    ]
                            ]
                        ]
                ]
                Html.divc "column" [
                    if aclType.Searchable |> Option.defaultValue false then
                        let v: string = snd searchState

                        Html.inputc "text" [
                            Attr.value v

                            match searchState with
                            | AcceptInput, _ -> autofocus
                            | SearchState.Searching, _ -> Attr.disabled true

                            onInput (fun e -> e.inputElement.value |> Msg.AclSearchChange |> dispatch) []
                        ]

                        bButton "Search" [
                            text "Search"
                            onClick
                                (fun _ ->
                                    AclParentMsg.SearchRequest {
                                        SearchText = v
                                        AclName = aclType.Name
                                    }
                                    |> dispatchParent)
                                []
                        ]
                ]
                Html.divc "column is-three-fifths" [
                    data_ "purpose" "params"
                    renderAclSearchResults aclParams aclType
                ]
            ]
        ]

    // render a display of the acl for a table-like thing, not an editor
    let renderAcl (props: AclProps) dispatch =
        if props.Acl.Name <> props.AclRef.Name then
            failwith $"renderAcl {props.Acl.Name} - {props.AclRef.Name}"

        let isActiveRow = props.IsSelected

        let isConfigurable =
            match props.Acl.ParameterType with
            | AclParameterType.None -> false
            | _ -> true

        printfn "'%s' - '%A' is configurable? '%A'" props.Acl.Name props.Acl.ParameterType isConfigurable

        let tButton title isActiveButton isPrimary isDisabled icon fOnClick =
            bButton title [
                Attr.classes [
                    "button"
                    "is-small"
                    if isPrimary then
                        "is-primary"
                    if isActiveButton then "is-light" else "is-link"
                ]
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
                    tButton "Edit Acl" isActiveRow true isActiveRow (App.Init.IconSearchType.MuiIcon "Edit") (fun _ ->
                        props.AclRef |> Some |> Msg.AclSelect |> dispatch
                        AclParentMsg.TypeChange props.Acl |> props.DispatchParent)
            ]
            Html.divc "column is-one-fifth buttonColumn" [
                tButton "Delete" isActiveRow false false (MuiIcon "Delete") (fun _ ->
                    AclParentMsg.Remove props.Acl |> props.DispatchParent)
            ]
            Html.divc "column is-four-fifths" [
                Attr.classes [
                    if isActiveRow then
                        "has-text-weight-bold"
                ]
                text props.AclRef.Name
                Html.spanc "info" [ text "*"; Attr.title (Core.pretty props.AclRef) ]
                Html.spanc "info" [ text "*"; Attr.title (Core.pretty props.Acl) ]
            ]
            Html.divc "column is-one-fifth" [
                if props.AclRef.Parameters.Length < 1 && isConfigurable then
                    Html.divc "is-warning" [ Attr.title "Not configured"; tryIcon (MuiIcon "Error") ]
                elif isConfigurable then
                    text (string props.AclRef.Parameters.Length)
                else
                    text ""
            ]
        ]

open Sutil.Styling
open type Feliz.length
open type Feliz.borderStyle

let css = [
    rule "span.info" Gen.CssRules.titleIndicator
    rule "h2" [ Css.borderBottom (px 1, solid, "black") ]
    rule ".fill .tabs" [ Css.width (percent 100) ]

    // not working
    // rule ".panel-block>*" [
    //     Css.custom("!width", "100%")
    // ]

    // rule "button.button" [
    // //     Css.marginBottom (px -15)
    // //     Css.marginTop (px -15)
    //     // Css.fontSize (em 0.7)
    //     Css.paddingTop (px 0)
    //     Css.paddingBottom (px 0)
    //     Css.marginBottom (px 0)
    //     Css.marginTop (px 0)
    // ]

    // rule "button.button>.icon" [
    //     // Css.fontSize (rem 0.1)
    //     Css.paddingTop (px 0)
    //     Css.paddingBottom (px 0)
    //     Css.marginBottom (px 0)
    //     Css.marginTop (px 0)
    // ]

    rule "div.buttonColumn" [
        Css.height (em 1.0)
        Css.width (em 2.5)
        Css.flexShrink 0
        Css.marginRight (px 5)
    ]

    rule ".columns" [ Css.marginTop (px 0); Css.marginBottom (px 0); Css.marginLeft (px 5) ]
]

type AclEditorProps = {
    ItemAcls: AclRef seq
    AclTypes: Acl seq
    ResolvedParams: System.IObservable<Map<string, AclDisplay>>
    AclSearchResponse: AclSearchResponse option
    DispatchParent: Dispatch<AclParentMsg>
}

// allow them to edit or create one
let renderAclsEditor (props: AclEditorProps) =
    if Core.windowData then
        printfn "AclEditor Render"

    toGlobalWindow "aclEditor_props" props

    let store, dispatch =
        props.ItemAcls
        |> Seq.tryHead
        |> Option.map (fun v -> { IsNew = false; AclRef = v })
        |> Store.makeElmish init (update props.ItemAcls) ignore

    toGlobalWindow "aclEditor_model" store.Value

    let m =
        props.ResolvedParams
        |> Observable.map (fun m -> m |> Map.map (fun _ v -> AclRefState.Response(Ok v)))

    Html.divc "fill" [
        disposeOnUnmount [ store ]
        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay
        data_ "file" "AclEditor"
        data_ "method" "renderAclsEditor"

        Bind.el (
            store |> Store.map MLens.getFocusedAcl,
            fun focusAclOpt ->

                let selectedType =
                    focusAclOpt
                    |> Option.map (fun item -> item.AclRef.Name)
                    |> Option.defaultValue "" // String.toLower store.Value.FocusedAcl.Name

                let selectedAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = selectedType)

                Html.div [
                    bButton "Add" [
                        tryIcon (App.Init.IconSearchType.MuiIcon "Add")
                        onClick (fun _ -> Msg.AclSelect None |> dispatch) []
                    ]
                    Renderers.renderAclTypeSelector
                        {
                            AclTypes = props.AclTypes
                            Focus = focusAclOpt
                            SelectedType = selectedType
                        }
                        dispatch
                    match focusAclOpt, selectedAclType with
                    | Some focusItem, Some acl when focusItem.IsNew ->
                        bButton "Add" [
                            tryIcon (App.Init.IconSearchType.MuiIcon "Save")
                            onClick (fun _ -> AclParentMsg.Create(focusItem.AclRef, acl) |> props.DispatchParent) []
                        ]
                    | _ -> ()

                    match focusAclOpt with
                    | Some focusedAcl ->
                        Html.spanc "info" [ text "*"; Attr.title (Core.pretty focusedAcl) ]
                        Html.spanc "info" [ text "*"; Attr.title (Core.pretty selectedAclType) ]

                        Html.divc "box" [
                            // let renderAclParams (idMap:Map<string,AclRefState>) (aclType: Acl) (item: AclRef) =
                            match selectedAclType with
                            | Some selectedAclType ->
                                let aclParams =
                                    if selectedAclType.Searchable |> Option.defaultValue false then
                                        props.AclSearchResponse
                                        |> Option.map (fun acl ->
                                            acl.Results |> Seq.map (fun v -> v.Reference, v.DisplayName))
                                    else
                                        selectedAclType.SelectableParameters |> Option.map (Seq.map Tuple2.replicate)
                                    |> Option.map List.ofSeq
                                    |> Option.defaultValue List.empty

                                Bind.el (
                                    m,
                                    (fun m ->
                                        Renderers.renderAclParams
                                            (store.Value.SearchState, store.Value.SearchText)
                                            aclParams
                                            m
                                            selectedAclType
                                            focusedAcl.AclRef
                                            dispatch
                                            props.DispatchParent)
                                )
                            | None -> ()
                        ]
                    | None -> ()

                ]

        )
        Html.h2 [ text "Existing Acls" ]

        Bind.el (
            store |> Store.map MLens.getFocusedAcl,
            fun focusOpt ->

                Html.div [
                    for (i, aclRef) in props.ItemAcls |> Seq.indexed do
                        let thisAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = aclRef.Name)

                        Html.div [
                            if i % 2 = 0 then
                                Attr.className "has-background-link-light"
                            match thisAclType with
                            | None -> Html.div [ text aclRef.Name ]
                            | Some aclType ->
                                Renderers.renderAcl
                                    {
                                        Acl = aclType
                                        AclRef = aclRef
                                        DispatchParent = props.DispatchParent
                                        IsSelected =
                                            let selectedType =
                                                focusOpt
                                                |> Option.map (fun item -> item.AclRef.Name)
                                                |> Option.defaultValue "" // String.toLower store.Value.FocusedAcl.Name

                                            props.AclTypes
                                            |> Seq.tryFind (fun v -> v.Name = selectedType)
                                            |> Option.map (fun aclType -> aclRef.Name = aclType.Name)
                                            |> Option.defaultValue false
                                    }
                                    dispatch
                        ]
                ]
        )
    ]
    |> withStyle css
