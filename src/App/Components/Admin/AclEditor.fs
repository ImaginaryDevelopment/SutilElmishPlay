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

type AclItemState =
    | ToUpsert of Set<AclRefId>
    | ToRemove

type AclState = {
    Original: Set<AclRefId> option
    // option so we can reset pending changes
    Next: Lazy<IStore<AclItemState option>>
}

type Model = {
    ResolvedAclStore: IStore<ResolvedAclLookup>
    // should be exhaustive of all acl names
    ItemAcls: Map<AclType, AclState>
    // option because we might not be editing/creating
    // all edits should be stored here, not on the map
    FocusedAclType: AclType option
    // should the date time be optional for only expiring errors?
    ErrorQueue: (string * System.DateTime) list
}

// type AclChangeType =
//     | Remove
//     | Upsert of Set<AclRefId>

module MLens =
    let getErrors model = model.ErrorQueue
    let getFocusedAclType model = model.FocusedAclType
    let makeAclStateNext value : Lazy<IStore<AclItemState option>> = Lazy(fun _ -> Store.make value)

    let getAclStore' aclType (store: IStore<Model>) =
        let makeItemStore () =
            let pStore = Store.make None |> Lazy.CreateFromValue

            store.Update(fun oldModel -> {
                oldModel with
                    ItemAcls = oldModel.ItemAcls |> Map.add aclType { Original = None; Next = pStore }
            })

            pStore

        store.Value.ItemAcls
        |> Map.tryFind aclType
        |> function
            | None ->
                eprintfn "AclName not found in exhaustive map: %A" aclType.Name
                makeItemStore ()
            // we found the acl type, but it has no value yet
            | Some value -> value.Next

    let getAclState aclType model = model.ItemAcls |> Map.find aclType


    let getParamsStore aclType model =
        let aclState = getAclState aclType model
        aclState.Next.Value

    let getAclParamsFromStateOpt aclType (aclState: AclState) =
        aclState.Next
        |> Lazy.ifCreated
        |> Option.bind (fun v -> v.Value)
        |> Option.bind (function
            | ToUpsert values -> Some values
            | _ -> None)
        |> Option.orElse aclState.Original

    // allows upsert and delete
    // adding an acl or updating one or removing
    let updateAcl aclType (next: AclItemState option) model =
        let iStore = getParamsStore aclType model
        // (getAclStore aclType store).Value.Update()
        // store to hold item changes for this navItem's acls
        iStore.Update(fun _ -> next)

    // toggle if aclRefId is included in acl's params
    let updateAclParamStore aclRefId (pStore: IStore<_>) =
        pStore.Update(
            function
            | None -> ToUpsert(Set.singleton aclRefId)
            | Some ToRemove -> ToUpsert(Set.singleton aclRefId)
            | Some(ToUpsert oldValues) ->
                if oldValues |> Set.contains aclRefId then
                    oldValues |> Set.remove aclRefId
                else
                    oldValues |> Set.add aclRefId
                |> ToUpsert
            >> Some
        )

    let updateAclParam (aclType: AclType) aclRefId model =
        let iStore = getParamsStore aclType model
        updateAclParamStore aclRefId iStore

type Msg =
    | AclSearchRequest of AclRefValueArgs
    | AclSearchResponse of Result<NavAclResolveResponse, ErrorType>
    | AclParamResolveRequest of AclRefLookup
    | AclParamResolveResponse of AclName * Result<NavAclResolveResponse, ErrorType>
    | AclSelection of AclName option
// | AclParamSelection of AclRefId
// | AclCreateRequest of AclType * Set<string>
// | AclRemoveRequest of AclType

module Commands =
    let getAclResolved token req =
        let f x =
            async {
                let! resp = Api.Shared.getAclReferenceDisplay token x

                match resp with
                | Ok v -> return Ok v
                | Error e -> return Error e
            }

        Cmd.OfAsync.either f req id (fun ex -> Choice2Of2 ex |> Error)
        |> Cmd.map (fun v -> Msg.AclParamResolveResponse(req.AclName, v))

let update (aclTypes: Map<AclName, AclType>) msg model : Model * Cmd<Msg> =
    printfn "AdminAclEditor update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | Msg.AclSelection None -> { model with FocusedAclType = None }, Cmd.none
    // | Msg.AclParamSelection aclRefId ->
    //     // should this change both focused acl and the map?
    //     match model |> MLens.getFocusedAclType with
    //     | None ->
    //         eprintfn "AclParam selected without a focused item"
    //         model, Cmd.none
    //     | Some fa ->
    //         model |> MLens.updateAclParam fa aclRefId
    //         model, Cmd.none

    | Msg.AclSelection(Some aclName) ->
        {
            model with
                FocusedAclType = aclTypes |> Map.tryFind aclName
        },
        Cmd.none

type AclEditorProps = {
    AclTypes: AclType seq
    ItemAcls: NavItemAclRefsMap
    ResolvedAclStoreOpt: IStore<ResolvedAclLookup> option
}

let init (props: AclEditorProps) : Model * Cmd<Msg> =
    let focusedAclTypeOpt =
        let firstAcl =
            props.ItemAcls
            |> Map.tryFindKey (fun _ _ -> true)
            |> Option.map (fun k -> k, props.ItemAcls[k])

        match firstAcl with
        | Some(acl, _) -> props.AclTypes |> Seq.tryFind (fun aT -> aT.Name = acl)
        | _ -> None

    let model = {
        ResolvedAclStore =
            props.ResolvedAclStoreOpt
            |> Option.defaultWith (fun () -> Map.empty |> Store.make)
        // aclState:
        // Original: Set<AclRefId> option
        // Next: Lazy<IStore<Set<AclRefId>>>
        // Map<AclType, AclState option>
        ItemAcls =
            let m =
                props.AclTypes
                |> Seq.map (fun aclType ->
                    let values =
                        props.ItemAcls
                        |> Map.tryFind aclType.Name
                        |> Option.map (Set.map AclRefId)
                        |> Option.map (fun p -> {
                            Original = Some p
                            Next = MLens.makeAclStateNext None
                        })
                        |> Option.defaultValue {
                            Original = None
                            Next = MLens.makeAclStateNext None
                        }

                    aclType, values)
                |> Map.ofSeq

            m
        FocusedAclType = focusedAclTypeOpt
        ErrorQueue = List.empty
    }

    match props.AclTypes |> Seq.length, model.ItemAcls.Count with
    | tCount, mCount when tCount = mCount -> ()
    | tCount, mCount -> failwith "Bad Map, types:%i, m:%i" tCount mCount

    model, Cmd.none

module Renderers =
    type AclTypeSelectorProps = {
        Focus: AclType option
        AclTypes: AclType seq
        ExistingAclTypes: AclName list
        SelectedType: AclName option
    }

    let renderAclTypeSelector (props: AclTypeSelectorProps) dispatch =
        // log aclTypes
        let disabled =
            match props.Focus with
            | None -> true
            | Some _ -> false

        selectInput
            {
                Values = props.AclTypes
                HasEmpty = true
                SelectType =
                    let selectedItem =
                        props.SelectedType
                        |> Option.bind (fun st -> props.AclTypes |> Seq.tryFind (fun a -> a.Name = st))

                    let valueMap =
                        Option.ofValueString
                        >> Option.bind (fun v -> props.AclTypes |> Seq.tryFind (fun a -> a.Name = AclName v))

                    StaticSelect(selectedItem, valueMap, ignore)
                ValueGetter = fun ({ Name = AclName name } as x) -> name
                NameGetter = fun { Name = AclName name } -> name
                OptionChildren = fun _ -> List.empty
            }
            []


    type AclProps = {
        AclParams: Set<AclRefId>
        AclType: AclType
        IsSelected: bool
    }

    // render a display of the acl for a table-like thing, not an editor
    let renderAcl (props: AclProps) dispatch =

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
                        props.AclType.Name |> Some |> Msg.AclSelection |> dispatch)
            ]
            Html.divc "column is-one-fifth buttonColumn" [
                tButton "Delete" isActiveRow false false (MuiIcon "Delete") (fun _ -> ())
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

    // all reference params at least at this time, are searchable
    let renderReferenceParams (aclType: AclType) (store: IStore<AclItemState option>) =
        Html.div [ formFieldAddons [] [] [] ]

    // items is all possible selectable items
    let renderSelectableParams aclType items (store: IStore<AclItemState option>) =
        printfn "renderSelectableParams"
        // select for param values to be included
        selectInput
            {
                Values = items |> Seq.map AclRefId
                HasEmpty = true
                ValueGetter = AclRefId.getText
                NameGetter = AclRefId.getText
                OptionChildren = fun child -> []
                SelectType =
                    if aclType.MultiValue then
                        // a list of selected AclRefIds

                        ObservedMulti(
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (function
                                 | None
                                 | Some ToRemove -> List.empty
                                 | Some(ToUpsert v) -> v |> Set.toList),
                            // Option.ofValueString
                            // >> Option.bind (fun aclName ->
                            //     props.AclTypes |> Seq.tryFind (fun a -> a.Name = AclName aclName)),
                            (fun ari -> store |> MLens.updateAclParamStore ari
                            // Msg.AclParamSelection ari |> dispatch)
                            )
                        )
                    else
                        let pStore =
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (function
                                 | None
                                 | Some ToRemove -> None
                                 | Some(ToUpsert v) -> v |> Set.toSeq |> Seq.tryHead)

                        ObservedSelect(
                            pStore,
                            Option.iter (fun aclRefId -> store |> MLens.updateAclParamStore aclRefId)
                        )

            }
            [ data_ "purpose" "param selector" ]

    type AclInterfaceProps = {
        AclType: AclType
        AclParams: System.IObservable<Set<AclRefId> option>
        // was this already in the api, or are we doing an upsert?
        Existed: bool
    }

    let renderAclInterface (props: AclInterfaceProps) model =
        printfn "renderAclInterface"
        let pStore = MLens.getParamsStore props.AclType model

        let updateAcl aclChangeType =
            model |> MLens.updateAcl props.AclType aclChangeType

        // account for multiples?
        let renderParamSelector (aclType: AclType) =
            // selector if not searchable
            // search and selector if searchable
            Bind.el (
                App.Global.resolvedAclLookup
                |> Store.map (Map.tryFind aclType.Name >> Option.defaultValue Map.empty),
                fun resolvedAclLookup ->
                    printfn "renderParamSelector"

                    match aclType.AclParamType with
                    | AclParameterType.None -> Html.div [ Attr.title "No Params" ]
                    | AclParameterType.Selectable items -> renderSelectableParams aclType items pStore
                    | AclParameterType.Reference _searchable -> renderReferenceParams aclType pStore
            )

        let renderAddButton () =
            let addText = "Add New Acl"

            bButton addText [
                data_ "button-purpose" addText
                tryIcon (IconSearchType.MuiIcon "Add")
                // text addText
                onClick (fun _ -> updateAcl (Some <| ToUpsert Set.empty)) []
            ]

        if props.AclType.AclParamType <> AclParameterType.None then

            columns2 [] [
                Html.h2 [
                    let titling =
                        let t = if props.Existed then "Editing" else "Creating"
                        $"{t} {props.AclType.Name}"

                    text titling
                ]
            ] [ renderParamSelector props.AclType ]
        else
            Html.div [
                if props.Existed then
                    let removeText = "Remove"

                    bButton removeText [
                        data_ "button-purpose" removeText
                        tryIcon (IconSearchType.MuiIcon "Remove")

                        onClick (fun _ -> updateAcl (Some ToRemove)) []
                    ]
                else
                    renderAddButton ()
            ]

let render (props: AclEditorProps) =
    let aclTypeMap =
        props.AclTypes |> Seq.map (fun aclType -> aclType.Name, aclType) |> Map.ofSeq

    let store, dispatch = props |> Store.makeElmish init (update aclTypeMap) ignore

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
        collapsibleCard None (text "Existing Acls") [
            CardContentType.Content [
                Bind.el2 (store |> Store.map MLens.getFocusedAclType) (store |> Store.map (fun v -> v.ItemAcls))
                <| fun (focusOpt, itemAcls) ->
                    printfn "Render existing with %i items" <| Map.count itemAcls

                    Html.div [
                        if Map.count itemAcls < 1 then
                            Html.divc "has-text-warning-light has-background-grey-light" [ text "No Acls present" ]
                        else
                            for (i, KeyValue(aclType, aclState)) in itemAcls |> Seq.indexed do
                                let thisAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = aclType.Name)

                                Html.div [
                                    if i % 2 = 0 then
                                        Attr.className "has-background-link-light"
                                    // render the selected Acl if there is one
                                    match thisAclType with
                                    | None -> Html.div [ text <| AclName.getText aclType.Name ]
                                    | Some aclType ->
                                        Renderers.renderAcl
                                            {
                                                AclType = aclType
                                                AclParams =
                                                    aclState
                                                    |> MLens.getAclParamsFromStateOpt aclType
                                                    |> Option.defaultValue Set.empty
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
                            Option.map (fun x -> x.Name) >> Msg.AclSelection >> dispatch
                        )
                }
                [ data_ "purpose" "aclTypeSelector" ]
            // acl editor/creator
            Bind.el (
                store |> Store.map (MLens.getFocusedAclType) |> Observable.distinctUntilChanged,
                function
                | None -> Html.div []
                | Some(aclType) ->
                    printfn "Render AclEditor.focusedChange?"

                    let existed = store.Value.ItemAcls |> Map.tryFind aclType |> Option.isSome

                    let pStore =
                        store.Value
                        |> MLens.getAclState aclType
                        |> fun x -> x.Next |> Lazy.get
                        |> Store.map (
                            Option.bind (function
                                | ToUpsert v -> Some v
                                | _ -> None)
                        )

                    // let pStore = store |> Store.map (MLens.getCurrentParams aclType.Name)

                    Renderers.renderAclInterface
                        {
                            AclType = aclType
                            Existed = existed
                            AclParams = pStore
                        }
                        store.Value
            )
        ]
    ]
    |> Styling.withCss
