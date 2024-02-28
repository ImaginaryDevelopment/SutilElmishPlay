module App.Components.Admin.AclEditor

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

// should not be able to have IsNew=False; AclValue = None
type AclState = {
    AclType: AclType
    AclParams: Set<string>
}

type Model = {
    ResolvedAclStore: IStore<ResolvedAclLookup>
    ItemAcls: NavItemAclRefsMap
    // option because we might not be editing/creating
    FocusedAcl: AclState option
    // should the date time be optional for only expiring errors?
    ErrorQueue: (string * System.DateTime) list
}

type AclChangeType =
    | Remove
    | Upsert of Set<string>

module MLens =
    let getErrors model = model.ErrorQueue
    let getFocusedAcl model = model.FocusedAcl
    // allows upsert and delete
    let updateAcl aclName valueOpt model =
        let nextAcls =
            match valueOpt with
            | Remove -> model.ItemAcls |> Map.remove aclName
            | Upsert p -> model.ItemAcls |> Map.add aclName p

        { model with ItemAcls = nextAcls }

type Msg =
    | AclSearchRequest of AclRefValueArgs
    | AclSearchResponse of Result<NavAclResolveResponse, ErrorType>
    | AclParamResolveRequest of AclRefLookup
    | AclParamResolveResponse of AclName * Result<NavAclResolveResponse, ErrorType>
    | AclSelection of AclName option
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


let update aclTypes msg model : Model * Cmd<Msg> =
    match msg with
    | Msg.AclSelection None -> { model with FocusedAcl = None }, Cmd.none

    | Msg.AclSelection(Some aclName) ->
        let itemAcl = model.ItemAcls |> Map.tryFind aclName
        let aclType = aclTypes |> Map.tryFind aclName

        match aclType, itemAcl with
        | Some aclType, Some aclValues ->
            // edit existing
            {
                model with
                    FocusedAcl =
                        Some {
                            // IsNew = false
                            AclType = aclType
                            AclParams = aclValues
                        }
            },
            Cmd.none
        | None, _ ->
            eprintfn "AclType not found for %A" aclName
            model, Cmd.none
        | Some aclType, None ->
            {
                model with
                    FocusedAcl =
                        Some {
                            // IsNew = true
                            AclType = aclType
                            AclParams = Set.empty
                        }
            },
            Cmd.none

type AclEditorProps = {
    AclTypes: AclType seq
    ItemAcls: NavItemAclRefsMap
    ResolvedAclStoreOpt: IStore<ResolvedAclLookup> option
}

let init (props: AclEditorProps) : Model * Cmd<Msg> =
    let focusedAclOpt =
        let firstAcl =
            props.ItemAcls
            |> Map.tryFindKey (fun _ _ -> true)
            |> Option.map (fun k -> k, props.ItemAcls[k])

        match firstAcl with
        | Some(acl, values) ->
            props.AclTypes
            |> Seq.tryFind (fun aT -> aT.Name = acl)
            |> function
                | Some aclType ->
                    Some {
                        // IsNew = false
                        AclType = aclType
                        AclParams = values
                    }
                | _ -> None
        | _ -> None

    let model = {
        ResolvedAclStore =
            props.ResolvedAclStoreOpt
            |> Option.defaultWith (fun () -> Map.empty |> Store.make)
        ItemAcls = props.ItemAcls
        FocusedAcl = focusedAclOpt
        ErrorQueue = List.empty
    }

    model, Cmd.none

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
            | Some f -> false

        // Html.select [
        //     text "Acl!"
        //     Attr.className "select"
        //     Attr.disabled disabled
        //     Handlers.onValueChangeIf dispatch (fun v ->
        //         Option.ofValueString v |> Option.map (AclName >> Msg.TypeSelectChange))
        //     Html.option [ text "" ]
        //     for ({ Name = AclName oName } as o) in
        //         props.AclTypes
        //         |> Seq.filter (fun { Name = aclName } ->
        //             props.SelectedType = Some aclName
        //             || props.ExistingAclTypes |> List.exists (fun eat -> eat = aclName) |> not) do
        //         Html.option [
        //             Attr.value oName
        //             text oName
        //             Attr.title <| pretty o
        //             if props.SelectedType = Some o.Name then
        //                 Attr.selected true
        //         ]
        // ]

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
        AclData: AclData
        AclType: AclType
        IsSelected: bool
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
                        props.AclData.Name |> Some |> Msg.AclSelection |> dispatch)
            ]
            Html.divc "column is-one-fifth buttonColumn" [
                tButton "Delete" isActiveRow false false (MuiIcon "Delete") (fun _ -> ())
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

    type AclInterfaceProps = {
        AclType: AclType
        HasParams: bool
        Exists: bool
    }

    let renderAclInterface (props: AclInterfaceProps) (store: IStore<Model>) =
        let updateAcl aclChangeType =
            store.Update(fun model -> model |> MLens.updateAcl props.AclType.Name aclChangeType)

        let renderAddButton () =
            let addText = "Add New Acl"

            bButton addText [
                data_ "button-purpose" addText
                tryIcon (IconSearchType.MuiIcon "Add")
                // text addText
                onClick (fun _ -> updateAcl (Upsert Set.empty)) []
            ]

        if props.HasParams then

            columns2 [] [
                Html.h2 [
                    let titling =
                        let t = if props.Exists then "Editing" else "Creating"
                        $"{t} {props.AclType.Name}"

                    text titling
                ]
            ] []
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

    Html.divc "fill" [
        disposeOnUnmount [ store ]
        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay
        data_ "file" "AclEditor"
        data_ "method" "renderAclsEditor"
        collapsibleCard None (text "Existing Acls") [
            CardContentType.Content [
                Bind.el2 (store |> Store.map MLens.getFocusedAcl) (store |> Store.map (fun v -> v.ItemAcls))
                <| fun (focusOpt, itemAcls) ->
                    printfn "Render existing with %i items" <| Map.count itemAcls

                    Html.div [
                        if Map.count itemAcls < 1 then
                            Html.divc "has-text-warning-light has-background-grey-light" [ text "No Acls present" ]
                        else
                            for (i, KeyValue(aclName, aclData)) in itemAcls |> Seq.indexed do
                                let thisAclType = props.AclTypes |> Seq.tryFind (fun v -> v.Name = aclName)

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
                                                AclData = { Name = aclName; Parameters = aclData }
                                                IsSelected =
                                                    focusOpt
                                                    |> Option.map (fun focus -> focus.AclType.Name = aclType.Name)
                                                    |> Option.defaultValue false
                                            }
                                            dispatch
                                ]
                    ]

            ]
        ]

        Html.divc "card" [
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
                                (MLens.getFocusedAcl >> Option.map (fun v -> v.AclType))

                        ObservedSelect(
                            rStore,
                            // Option.ofValueString
                            // >> Option.bind (fun aclName ->
                            //     props.AclTypes |> Seq.tryFind (fun a -> a.Name = AclName aclName)),
                            Option.map (fun x -> x.Name) >> Msg.AclSelection >> dispatch
                        )
                }
                []

            Bind.el (
                store |> Store.map MLens.getFocusedAcl,
                function
                | None -> Html.div []
                | Some({
                           AclType = { Name = AclName name } as aclType
                       } as focus) ->
                    let hasParams =
                        match focus.AclType.AclParamType with
                        | AclParameterType.Selectable _ -> true
                        | AclParameterType.Reference _ -> true
                        | _ -> false

                    let isSearchable =
                        hasParams
                        && match focus.AclType.AclParamType with
                           | AclParameterType.Reference true -> true
                           | _ -> false

                    Bind.el (
                        store |> Store.map (fun model -> model.ItemAcls |> Map.tryFind (AclName name)),
                        fun x ->
                            let exists = store.Value.ItemAcls |> Map.containsKey (AclName name)

                            Renderers.renderAclInterface
                                {
                                    AclType = aclType
                                    HasParams = hasParams
                                    Exists = exists
                                }
                                store
                    )
            )
        ]
    ]
    |> Styling.withCss
