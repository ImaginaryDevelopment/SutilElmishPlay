module App.Components.Admin.AclEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Adapters
open App.Adapters.Schema

open App.Adapters.Api.Schema
open App.Adapters.Api.Shared

open App.Adapters.Icons
open App.Adapters.Html
open App.Adapters.Bulma

open App.Components
open App.Components.Admin.AclTypeEditor

open Core

module Handlers = App.Adapters.Html.Handlers

module Styling =
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
        rule ".select-multi" [ Css.minHeight (px 200); Css.minWidth (px 250) ]

        rule ".columns" [ Css.marginTop (px 0); Css.marginBottom (px 0); Css.marginLeft (px 5) ]
    ]

    let withCss = withStyle css


// dimensions:
// acl is present
// has params
// slated changes
// proper map would be AclName/AclType, AclType * Set option * AclExistState

// type AclOriginalState =
//     | WasNotPresent
//     | WasPresent of Set<AclRefId>

// type AclItemState =
//     | ToUpsert of Set<AclRefId>
//     | ToRemove

// // states the display grid is concerned with
// //
// type AclMapState =
//     | UnchangedPresent
//     | Changed
//     | Removing

// type AclState = {
//     AclOriginalState: AclOriginalState
//     // option so we can reset pending changes
//     Next: AclItemState option
// }

// should be exhaustive of all acl names
// type ItemAclStore = IStore<Map<AclType, AclState>>

// type ItemAclTypeStore = IStore<Map<AclRefId, AclDisplay>
type Model = {
    ResolvedAclStore: IStore<ResolvedAclLookup>
    SearchIsInFlight: bool
    // option because we might not be editing/creating
    // all edits should be stored here, not on the map
    FocusedAclType: AclType option
    // should the date time be optional for only expiring errors?
    ErrorQueue: (string * System.DateTime) list
}

module MLens =
    let getErrors model = model.ErrorQueue
    let getFocusedAclType model = model.FocusedAclType
// let makeAclStateNext value : Lazy<IStore<AclItemState>> = Lazy(fun _ -> Store.make value)

// let getAclExistMap (aclState: Map<AclType, AclState>) : Map<AclType, AclMapState * Set<AclRefId>> =
//     aclState
//     |> Map.choose (fun aclType aclState ->
//         match aclState.AclOriginalState, aclState.Next with
//         | AclOriginalState.WasNotPresent, None -> None
//         | AclOriginalState.WasPresent v, None -> Some(AclMapState.UnchangedPresent, v)
//         | AclOriginalState.WasPresent v, Some AclItemState.ToRemove -> Some(AclMapState.Removing, v)
//         // this case should not happen
//         | AclOriginalState.WasNotPresent, Some ToRemove -> None
//         | _, Some(ToUpsert v) -> Some(AclMapState.Changed, v)
//         |> Option.map (fun x -> aclType, x))

// let getAclParamsFromState (aclState: AclState) =
//     aclState.Next
//     |> function
//         | Some(ToUpsert values) -> Some values
//         | Some ToRemove -> None
//         | None ->
//             aclState.AclOriginalState
//             |> function
//                 | WasPresent v -> Some v
//                 | _ -> None


type Msg =
    | AclTypeSelection of AclType option
    | AclRemove of AclType

type AclEditorProps = {
    Token: string
    AclTypes: AclType seq
    // not exhaustive, hence Set instead of Set option
    ItemAcls: IStore<Map<AclName, Set<AclRefId>>>
    ResolvedAclStoreOpt: IStore<ResolvedAclLookup> option
}

let init (props: AclEditorProps) : Model * Cmd<Msg> =
    let focusedAclTypeOpt =
        let firstAcl =
            props.ItemAcls.Value
            |> Map.tryFindKey (fun _ _ -> true)
            |> Option.map (fun k -> k, props.ItemAcls.Value[k])

        match firstAcl with
        | Some(acl, _) -> props.AclTypes |> Seq.tryFind (fun aT -> aT.Name = acl)
        | _ -> None

    let model = {
        ResolvedAclStore =
            props.ResolvedAclStoreOpt
            |> Option.defaultWith (fun () -> Map.empty |> Store.make)
        SearchIsInFlight = false
        // aclState:
        // Original: Set<AclRefId> option
        // Next: Lazy<IStore<Set<AclRefId>>>
        // Map<AclType, AclState option>
        // ItemAcls =
        //     let m =
        //         props.AclTypes
        //         |> Seq.map (fun aclType ->
        //             let values =
        //                 props.ItemAcls.Value
        //                 |> Map.tryFind aclType.Name
        //                 |> Option.map (Set.map AclRefId)
        //                 |> Option.map (fun p -> {
        //                     AclOriginalState = WasPresent p
        //                     Next = None
        //                 })
        //                 |> Option.defaultValue {
        //                     AclOriginalState = WasNotPresent
        //                     Next = None
        //                 }

        //             aclType, values)
        //         |> Map.ofSeq

        // m
        FocusedAclType = focusedAclTypeOpt
        ErrorQueue = List.empty
    }

    // match props.AclTypes |> Seq.length, props.ItemAcls.Value.Count with
    // | tCount, mCount when tCount = mCount -> ()
    // | tCount, mCount -> failwithf "Bad Map, types:%i, m:%i" tCount mCount

    model, Cmd.none

let private update token (itemAcls: IStore<Map<AclName, Set<AclRefId>>>) msg model =
    match msg with
    | Msg.AclTypeSelection x -> { model with FocusedAclType = x }, Cmd.none
    | Msg.AclRemove aclType ->
        itemAcls.Update(Map.change aclType.Name (fun _ -> None))
        model, Cmd.none

module Renderers =

    type AclDisplayProps = {
        AclParams: Set<AclRefId>
        AclType: AclType
        IsSelected: bool
    }

    // render a display of the acl for a table-like thing, not an editor
    let renderAclDisplay (props: AclDisplayProps) dispatch =

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
                        props.AclType |> Some |> Msg.AclTypeSelection |> dispatch)
            ]
            Html.divc "column is-one-fifth buttonColumn" [
                tButton "Delete" isActiveRow false false (MuiIcon "Delete") (fun _ ->
                    Msg.AclRemove props.AclType |> dispatch)
            ]
            Html.divc "column is-four-fifths" [
                Attr.classes [
                    if isActiveRow then
                        "has-text-weight-bold"
                ]
                text (AclName.getText props.AclType.Name)
                Html.spanc "info" [ text "*"; Attr.title (Core.pretty props.AclType) ]
                Html.spanc "info" [ text "*"; Attr.title (Core.pretty props.AclParams) ]
            ]
            Html.divc "column is-one-fifth" [
                if props.AclParams.Count < 1 && isConfigurable then
                    Html.divc "is-warning" [ Attr.title "Not configured"; tryIcon (MuiIcon "Error") ]
                elif isConfigurable then
                    text (string props.AclParams.Count)
                else
                    text ""
            ]
        ]

    let renderExistingAclsCard props store dispatch =
        collapsibleCard (Some(Choice2Of2 true)) (text "Existing Acls") [
            CardContentType.Content [
                Bind.el2 (store |> Store.map MLens.getFocusedAclType) props.ItemAcls
                <| fun (focusOpt, itemAcls) ->
                    printfn "Render existing with %i items" <| Map.count itemAcls

                    Html.div [
                        if Map.count itemAcls < 1 then
                            Html.divc "has-text-warning-light has-background-grey-light" [ text "No Acls present" ]
                        else
                            for (i, KeyValue(aclName, p)) in itemAcls |> Seq.indexed do
                                let thisAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = aclName)

                                Html.div [
                                    if i % 2 = 0 then
                                        Attr.className "has-background-link-light"
                                    // render the selected Acl if there is one
                                    match thisAclType with
                                    | None -> Html.div [ text <| AclName.getText aclName ]
                                    | Some aclType ->
                                        renderAclDisplay
                                            {
                                                AclType = aclType
                                                AclParams = p
                                                IsSelected =
                                                    focusOpt
                                                    |> Option.map (fun focus -> focus.Name = aclType.Name)
                                                    |> Option.defaultValue false
                                            }
                                            dispatch
                                ]
                    ]

            ]
        ]


let render (props: AclEditorProps) =

    let store, dispatch =
        props |> Store.makeElmish init (update props.Token props.ItemAcls) ignore

    let updateModel () =
        toGlobalWindow "aclEditor_model" store.Value |> ignore

    updateModel ()

    printfn "render AclEditor"

    Html.divc "fill" [
        disposeOnUnmount [ store ]
        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay
        data_ "file" "AclEditor"
        data_ "method" "renderAclsEditor"
        // existing acls section
        Renderers.renderExistingAclsCard props store dispatch
        // type selector and params manipulation
        Html.divc "card" [
            // type selector
            selectInput
                {
                    Values = props.AclTypes
                    HasEmpty = true
                    ValueGetter =
                        function
                        | { Name = AclName n } -> n
                    NameGetter =
                        function
                        | { Name = AclName n } -> n
                    OptionChildren = fun child -> []
                    SelectType =
                        let rStore =
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (MLens.getFocusedAclType)

                        ObservedSelect(
                            rStore,
                            // Option.ofValueString
                            // >> Option.bind (fun aclName ->
                            //     props.AclTypes |> Seq.tryFind (fun a -> a.Name = AclName aclName)),
                            Msg.AclTypeSelection >> dispatch
                        )
                }
                [ data_ "purpose" "aclTypeSelector" ]

            // acl editor/creator
            Bind.el (
                store |> Store.map (MLens.getFocusedAclType) |> Observable.distinctUntilChanged,
                function
                | None -> Html.div []
                | Some aclType ->
                    // we're trying to map a Map<AclName,Set<_>> to Map<AclName,Set<_> option>
                    let getter m : Option<Set<AclRefId>> = m |> Map.tryFind aclType.Name

                    let setter (next: Option<Set<AclRefId>>) (oldParent, _) : Map<AclName, Set<AclRefId>> =
                        oldParent |> Map.change aclType.Name (fun _ -> next)

                    let iaStore: IStore<Set<AclRefId> option> =
                        props.ItemAcls
                        |> Store.mapStore "ItemAcls" true { Getter = getter; Setter = setter }

                    renderAclTypeEditor {
                        AclType = aclType
                        AclParams = iaStore
                        Token = props.Token
                    }
            )
        ]
    ]
    |> Styling.withCss
