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

type AclRefState =
    | Requested
    | Response of Result<AclDisplay, Gen.ErrorType>

type AclParentMsg =
    // which Acl Type is highlighted
    // None is a valid option
    | AclTypeChange of string
    | AclSearchRequest of AclRefValueArgs

type Msg =
    | TypeSelectChange of string
    | AclSelect of AclRef option
    | AclSearchChange of string

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

let stateStore: LocalStorage.IAccessor<CachedState> =
    LocalStorage.StorageAccess("AclEditor_CachedState")

let emptyAcl = { Name = ""; Parameters = Array.empty }
let inline justModel m = m, Cmd.none

let init aclRefOpt : Model * Cmd<Msg> =
    let searchText =
        stateStore.TryGetValue()
        |> Option.bind (fun cs -> cs.AclSearchText |> Option.ofValueString)
        |> Option.defaultValue ""

    justModel {
        FocusedAcl = aclRefOpt
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
    let justError title = MLens.addError title model, Cmd.none

    match msg, model with
    // blocks

    | AclSearchChange _, { SearchState = Searching } -> justError "Server is busy"
    // | AclSearchRequest, {SearchState = Searching, _} -> justError "A Search is already in flight"
    | TypeSelectChange _, { FocusedAcl = None } -> justError "Start a new acl first"
    | TypeSelectChange _, { FocusedAcl = Some { IsNew = false } } -> justError "Cannot change type on new acl"
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
        printfn "AclSelected"

        justModel { model with FocusedAcl = None }
    |> fun (next, cmd) ->
        let next =
            match SideEffects.saveStateCache next with
            | Ok() -> next
            | Error e -> MLens.addError e model

        next, cmd

module Renderers =
    open Gen

    let renderAclTypeSelector (aclTypes: Acl seq) selectedType disabled (dispatch: Dispatch<Msg>) =
        // log aclTypes

        Html.select [
            text "Acl!"
            Attr.className "select"
            Attr.disabled disabled
            Handlers.onValueChangeIf dispatch (fun v -> Option.ofValueString v |> Option.map Msg.TypeSelectChange)
            Html.option [ text "" ]
            for o in aclTypes do
                Html.option [
                    Attr.value o
                    text o.Name
                    if selectedType = o.Name then
                        Attr.selected true
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
            Html.divc "columns" [
                Html.divc "column" [
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

                        Html.buttonc "button" [
                            text "Search"
                            onClick
                                (fun _ ->
                                    AclParentMsg.AclSearchRequest {
                                        SearchText = v
                                        AclName = aclType.Name
                                    }
                                    |> dispatchParent)
                                []
                        ]
                ]
                Html.divc "column" [
                    let ps =
                        match aclParams with
                        | [] ->
                            aclType.SelectableParameters
                            |> Option.defaultValue Array.empty
                            |> Seq.map (fun p -> p, p)
                            |> List.ofSeq
                        | values -> values

                    Html.ul [
                        for (value, name) in ps do
                            Html.li [ text name; Attr.title value ]
                    ]
                ]
            ]
        ]

    let renderAcl selectedType (item: AclRef) dispatch dispatchParent =
        let isActiveButton = item.Name = selectedType

        Html.divc "columns" [
            Html.divc "column is-one-fifth buttonColumn" [
                Html.button [
                    Attr.title "Edit Acl"
                    Attr.classes [
                        "button"
                        "is-small"
                        if isActiveButton then "is-light is-primary" else "is-link"
                    ]
                    tryIcon (App.Init.IconSearchType.MuiIcon "Edit")
                    Attr.disabled isActiveButton
                    if not isActiveButton then
                        onClick
                            (fun _ ->
                                item |> Some |> Msg.AclSelect |> dispatch
                                AclParentMsg.AclTypeChange item.Name |> dispatchParent)
                            List.empty
                ]
            ]
            Html.divc "column is-four-fifths" [
                Attr.classes [
                    if isActiveButton then
                        "has-text-weight-bold"
                ]
                text item.Name
            ]
            Html.divc "column is-one-fifth" [ text (string item.Parameters.Length) ]
        ]

open Sutil.Styling
open type Feliz.length
open type Feliz.borderStyle

let css = [
    rule "span.info" Gen.CssRules.titleIndicator
    rule "h2" [ Css.borderBottom (px 1, solid, "black") ]

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
    SearchResults: (string * string) list
    DispatchParent: Dispatch<AclParentMsg>
}

// allow them to edit or create one
let renderAclsEditor (aea: AclEditorProps) =
    printfn "AclEditor Render"

    let store, dispatch =
        aea.ItemAcls
        |> Seq.tryHead
        |> Option.map (fun v -> { IsNew = false; AclRef = v })
        |> Store.makeElmish init (update aea.ItemAcls) ignore

    let m =
        aea.ResolvedParams
        |> Observable.map (fun m -> m |> Map.map (fun _ v -> AclRefState.Response(Ok v)))

    Html.div [
        disposeOnUnmount [ store ]
        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay

        Bind.el (
            store |> Store.map MLens.getFocusedAcl,
            fun focusAclOpt ->
                let selectedType =
                    focusAclOpt
                    |> Option.map (fun item -> item.AclRef.Name)
                    |> Option.defaultValue "" // String.toLower store.Value.FocusedAcl.Name

                let selectedAclType = aea.AclTypes |> Seq.tryFind (fun v -> v.Name = selectedType)

                Html.div [
                    Html.buttonc "button" [
                        tryIcon (App.Init.IconSearchType.MuiIcon "Add")
                        onClick (fun _ -> Msg.AclSelect None |> dispatch) []
                    ]
                    Renderers.renderAclTypeSelector aea.AclTypes selectedType (Option.isSome focusAclOpt) dispatch
                    match focusAclOpt with
                    | Some focusedAcl ->
                        Html.spanc "info" [ text "*"; Attr.title (Core.pretty focusedAcl) ]
                        Html.spanc "info" [ text "*"; Attr.title (Core.pretty selectedAclType) ]

                        Html.divc "box" [
                            // let renderAclParams (idMap:Map<string,AclRefState>) (aclType: Acl) (item: AclRef) =
                            match selectedAclType with
                            | Some selectedAclType ->
                                Bind.el (
                                    m,
                                    (fun m ->
                                        Renderers.renderAclParams
                                            (store.Value.SearchState, store.Value.SearchText)
                                            aea.SearchResults
                                            m
                                            selectedAclType
                                            focusedAcl.AclRef
                                            dispatch
                                            aea.DispatchParent)
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
                let selectedType =
                    focusOpt |> Option.map (fun item -> item.AclRef.Name) |> Option.defaultValue "" // String.toLower store.Value.FocusedAcl.Name

                Html.div [
                    for (i, acl) in aea.ItemAcls |> Seq.indexed do
                        Html.div [
                            if i % 2 = 0 then
                                Attr.className "has-background-link-light"
                            Renderers.renderAcl selectedType acl dispatch aea.DispatchParent
                        ]
                ]
        )

    ]
    |> withStyle css
