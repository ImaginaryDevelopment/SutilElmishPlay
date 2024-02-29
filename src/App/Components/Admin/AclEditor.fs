module App.Components.Admin.AclEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
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

        rule ".columns" [ Css.marginTop (px 0); Css.marginBottom (px 0); Css.marginLeft (px 5) ]
    ]

    let withCss = withStyle css

type Model = {
    ResolvedAclStore: IStore<ResolvedAclLookup>
    // this is a map of
    ItemAcls: Map<AclName, Set<AclRefId> * Lazy<IStore<Set<AclRefId>>>>
    // option because we might not be editing/creating
    // all edits should be stored here, not on the map
    FocusedAclType: AclType option
    // should the date time be optional for only expiring errors?
    ErrorQueue: (string * System.DateTime) list
}

type AclChangeType =
    | Remove
    | Upsert of Set<AclRefId>

module MLens =
    let getErrors model = model.ErrorQueue
    let getFocusedAclType model = model.FocusedAclType

    let getCurrentParams aclName model =
        model.ItemAcls
        |> Map.tryFind aclName
        |> Option.map (fun (original, lazyStore) ->
            let aclParams =
                if lazyStore.IsValueCreated then
                    lazyStore.Value.Value
                else
                    original

            aclParams)

    // allows upsert and delete
    // adding an acl or updating one or removing
    let updateAcl aclName valueOpt model =
        match valueOpt with
        | Remove ->
            let nextItemAcls =
                model.ItemAcls
                |> Map.change
                    aclName
                    (Option.bind (fun (_, lStore) ->
                        if lStore.IsValueCreated then
                            lStore.Value.Dispose()

                        None))

            { model with ItemAcls = nextItemAcls }
        | Upsert p ->
            // if the params already exist we don't actually need to change anything
            match model.ItemAcls |> Map.tryFind aclName with
            | None -> {
                model with
                    ItemAcls =
                        model.ItemAcls
                        |> Map.add aclName (Set.empty, Lazy.Create(fun () -> Store.make p))
              }
            | Some(_, lStore) ->
                lStore.Value.Update(fun _ -> p)
                model

    // toggle if aclRefId is included in acl's params
    let updateAclParam (aclType: AclType) aclRefId (model: Model) =
        match model.ItemAcls |> Map.tryFind aclType.Name with
        | None -> {
            model with
                ItemAcls =
                    model.ItemAcls
                    |> Map.add aclType.Name (Set.empty, Lazy.Create(fun () -> Store.make (Set.singleton aclRefId)))
          }
        | Some(_, lStore) ->
            lStore.Value.Update(fun values ->
                if values.Contains aclRefId then
                    values |> Set.remove aclRefId
                else
                    values |> Set.add aclRefId)

            model

type Msg =
    | AclSearchRequest of AclRefValueArgs
    | AclSearchResponse of Result<NavAclResolveResponse, ErrorType>
    | AclParamResolveRequest of AclRefLookup
    | AclParamResolveResponse of AclName * Result<NavAclResolveResponse, ErrorType>
    | AclSelection of AclName option
    | AclParamSelection of AclRefId
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
    | Msg.AclParamSelection aclRefId ->
        // should this change both focused acl and the map?
        match model |> MLens.getFocusedAclType with
        | None ->
            eprintfn "AclParam selected without a focused item"
            model, Cmd.none
        | Some fa -> model |> MLens.updateAclParam fa aclRefId, Cmd.none

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
        | Some(acl, values) -> props.AclTypes |> Seq.tryFind (fun aT -> aT.Name = acl)
        | _ -> None

    let model = {
        ResolvedAclStore =
            props.ResolvedAclStoreOpt
            |> Option.defaultWith (fun () -> Map.empty |> Store.make)
        ItemAcls =
            props.ItemAcls
            |> Map.map (fun _ v ->
                let refSet = v |> Set.map AclRefId
                refSet, Lazy.Create(fun () -> Store.make refSet))
        FocusedAclType = focusedAclTypeOpt
        ErrorQueue = List.empty
    }

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

    let renderReferenceParams (aclType: AclType) searchable store dispatch =
        Html.div [
            if searchable then
                formFieldAddons [] [] []
        ]

    let renderSelectableParams aclType items store dispatch =
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
                        let rStore =
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                // not cross referenced with resolved acls
                                (fun v ->

                                    v
                                    |> MLens.getFocusedAclType
                                    |> Option.bind (fun at ->

                                        store.Value |> MLens.getCurrentParams at.Name)
                                    |> Option.map Set.toList
                                    |> Option.defaultValue List.empty)

                        rStore.Add(fun v -> printfn "rStore updated to: %A" v)

                        ObservedMulti(
                            rStore,
                            // Option.ofValueString
                            // >> Option.bind (fun aclName ->
                            //     props.AclTypes |> Seq.tryFind (fun a -> a.Name = AclName aclName)),
                            (fun ari -> Msg.AclParamSelection ari |> dispatch)
                        )
                    else
                        let selectedParamStore = None |> Store.make

                        ObservedSelect(
                            selectedParamStore,
                            Option.iter (fun aclRefId ->
                                selectedParamStore.Update(fun _ -> Some aclRefId)
                                Msg.AclParamSelection aclRefId |> dispatch)
                        )

            }
            [ data_ "purpose" "param selector" ]

    type AclInterfaceProps = {
        AclType: AclType
        AclParams: System.IObservable<Set<AclRefId> option>
        Exists: bool
    }

    let renderAclInterface (props: AclInterfaceProps) (store: IStore<Model>) dispatch =
        printfn "renderAclInterface"

        let updateAcl aclChangeType =
            store.Update(fun model -> model |> MLens.updateAcl props.AclType.Name aclChangeType)

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
                    | AclParameterType.Selectable items -> renderSelectableParams aclType items store dispatch
                    | AclParameterType.Reference searchable -> renderReferenceParams aclType searchable store dispatch
            )

        let renderAddButton () =
            let addText = "Add New Acl"

            bButton addText [
                data_ "button-purpose" addText
                tryIcon (IconSearchType.MuiIcon "Add")
                // text addText
                onClick (fun _ -> updateAcl (Upsert Set.empty)) []
            ]

        if props.AclType.AclParamType <> AclParameterType.None then

            columns2 [] [
                Html.h2 [
                    let titling =
                        let t = if props.Exists then "Editing" else "Creating"
                        $"{t} {props.AclType.Name}"

                    text titling
                ]
            ] [ renderParamSelector props.AclType ]
        else
            Html.div [
                if props.Exists then
                    let removeText = "Remove"

                    bButton removeText [
                        data_ "button-purpose" removeText
                        tryIcon (IconSearchType.MuiIcon "Remove")

                        onClick
                            (fun _ -> store.Update(fun model -> model |> MLens.updateAcl props.AclType.Name Remove)

                            )
                            []
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
                            for (i, KeyValue(aclName, (existing, lazyStore))) in itemAcls |> Seq.indexed do
                                let thisAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = aclName)

                                let aclParams =
                                    if lazyStore.IsValueCreated then
                                        lazyStore.Value.Value
                                    else
                                        existing

                                Html.div [
                                    if i % 2 = 0 then
                                        Attr.className "has-background-link-light"
                                    // render the selected Acl if there is one
                                    match thisAclType with
                                    | None -> Html.div [ text <| AclName.getText aclName ]
                                    | Some aclType ->
                                        Renderers.renderAcl
                                            {
                                                AclType = aclType
                                                AclParams = aclParams
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

                    let hasParams =
                        match aclType.AclParamType with
                        | AclParameterType.Selectable _ -> true
                        | AclParameterType.Reference _ -> true
                        | _ -> false

                    // let isSearchable =
                    //     hasParams
                    //     && match focus.AclType.AclParamType with
                    //        | AclParameterType.Reference true -> true
                    //        | _ -> false
                    let exists = store.Value.ItemAcls |> Map.containsKey (aclType.Name)
                    // store |> Store.map (fun model -> model.ItemAcls |> Map.tryFind (AclName name)),
                    let pStore = store |> Store.map (MLens.getCurrentParams aclType.Name)

                    Renderers.renderAclInterface
                        {
                            AclType = aclType
                            Exists = exists
                            AclParams = pStore
                        }
                        store
                        dispatch
            )
        ]
    ]
    |> Styling.withCss
