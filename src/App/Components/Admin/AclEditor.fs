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

let diagnoseAclEditor = false

let dPrintfn txt =
    if diagnoseAclEditor then printfn "%s" txt else ()

module Styling =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle

    let css = [
        // rule "span.info" Gen.CssRules.titleIndicator
        rule "h2" [ Css.borderBottom (px 1, solid, "black") ]
        // rule ".field.has-addons .control select" [ Css.minHeight (px 40) ]

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
    let setFocusedAclType fat model = { model with FocusedAclType = Some fat }
    let clearFocusedAclType model = { model with FocusedAclType = None }

    let getUnresolvedParams (aclType: AclType) p =
        if Set.count p > 0 then
            match App.Global.resolvedAclLookup.Value |> Map.tryFind aclType.Name with
            | Some m ->
                let pUnresolved = p |> Set.filter (fun v -> m |> Map.containsKey v |> not)
                dPrintfn $"Filtered %i{p.Count} to %i{pUnresolved.Count} unresolved"
                if pUnresolved.Count > 0 then Some pUnresolved else None
            | None ->
                dPrintfn
                    $"ResolvedAcls found for %A{aclType.Name} - %i{Set.count p}(%i{App.Global.resolvedAclLookup.Value.Count})"

                Some p
        else
            None


type Msg =
    | AclTypeSelection of (NavId * AclType) option
    | AclRemove of AclType
    | AclParamResolveResponse of AclName * Result<AclDisplay, ErrorType>
    | AclBulkResolveResponse of AclName * Result<NavAclsResolveResponse, ErrorType>


module Commands =
    let getAclResolved token req =

        let f = Api.Shared.getAclReferenceDisplay token

        Cmd.OfAsync.either f req id (fun ex -> Choice2Of2 ex |> Error)
        |> Cmd.map (fun v -> Msg.AclParamResolveResponse(req.AclName, v))

    // bulk resolve all refIds on this existing navItem+aclName combo
    // this should only be searchable reference acl types
    let resolveAclsForExistingType token req : Cmd<Msg> =
        // TODO: return command none if the aclType is not a searchable reference
        match Api.Shared.getNavAclResolve token req with
        | Ok t ->
            Cmd.OfAsync.either (fun () -> t) () id Error
            |> Cmd.map (fun v ->
                let mapped = v |> Result.mapError (fun v -> Choice2Of2 v)
                Msg.AclBulkResolveResponse(req.AclName, mapped))
        | Error e ->
            eprintfn "%A" e

            Msg.AclBulkResolveResponse(req.AclName, Array.singleton e |> Choice1Of2 |> Error)
            |> Cmd.ofMsg

    let getAclsResolved token (aclType: AclType) refIds =
        (List.empty, refIds)
        ||> Seq.fold (fun commands refId ->
            getAclResolved token {
                AclName = aclType.Name
                AclType = aclType.AclParamType
                AclRefId = refId
            }
            :: commands)
        |> Cmd.batch

type AclEditorProps = {
    Token: string
    NavId: NavId
    AclTypes: AclType seq
    // not exhaustive, hence Set instead of Set option
    ItemAcls: IStore<Map<AclName, Set<AclRefId>>>
    ResolvedAclStoreOpt: IStore<ResolvedAclLookup> option
}

let handleUnresolvedParamsIfFound token navId (aclType: AclType) (itemAcls: IStore<Map<AclName, Set<AclRefId>>>) =
    // scrape out params that are already present in the lookup
    // TODO: don't send out requests for things already in flight?
    let pUnresolved =
        let existingP = itemAcls.Value
        dPrintfn $"Found %i{existingP.Count} existing acls"

        existingP
        |> Map.tryFind aclType.Name
        |> Option.bind (fun v ->
            dPrintfn $"Found %i{v.Count} existing params"
            v |> MLens.getUnresolvedParams aclType)
        |> Option.defaultValue Set.empty

    let hasUnresolved = Set.count pUnresolved > 0
    dPrintfn $"Are there unresolved params? %b{hasUnresolved}"

    if hasUnresolved then

        if NavIds.isValid navId then
            let cmd: Cmd<Msg> =
                Commands.resolveAclsForExistingType token {
                    AclName = aclType.Name
                    AclParameterType = aclType.AclParamType
                    NavId = navId
                }

            cmd
        else
            // this should probably never happen, how would we be getting a new nav item with existing searchable reference params

            printfn "Attempting to resolve %i params" pUnresolved.Count
            let cmd: Cmd<Msg> = Commands.getAclsResolved token aclType pUnresolved
            cmd
    else
        Cmd.none

let init (props: AclEditorProps) : Model * Cmd<Msg> =
    let focusedAclTypeOpt = None

    let cmd =
        focusedAclTypeOpt
        |> Option.map (fun aclType -> handleUnresolvedParamsIfFound props.Token props.NavId aclType props.ItemAcls)
        |> Option.defaultValue Cmd.none

    let model = {
        ResolvedAclStore =
            props.ResolvedAclStoreOpt
            |> Option.defaultWith (fun () -> Map.empty |> Store.make)
        SearchIsInFlight = false
        FocusedAclType = focusedAclTypeOpt
        ErrorQueue = List.empty
    }

    model, cmd


let private update token (itemAcls: IStore<Map<AclName, Set<AclRefId>>>) msg model : Model * Cmd<Msg> =
    printfn "AclEditor update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | Msg.AclParamResolveResponse(aclName, Error e) ->
        eprintfn "%A - %A" aclName e
        model, Cmd.none

    | Msg.AclTypeSelection None -> model |> MLens.clearFocusedAclType, Cmd.none

    | Msg.AclTypeSelection(Some(navId, aclType)) ->
        let next = model |> MLens.setFocusedAclType aclType
        next, handleUnresolvedParamsIfFound token navId aclType itemAcls

    | Msg.AclParamResolveResponse(aclName, Ok aclDisplay) ->
        printfn "AclParamResolveResponse: %A-%A '%s'" aclName aclDisplay.Reference aclDisplay.DisplayName
        // error printing is already done
        App.Global.ResolvedAclLookup.addValue aclName aclDisplay

        model, Cmd.none
    | Msg.AclBulkResolveResponse(aclName, Ok v) ->
        App.Global.ResolvedAclLookup.addValues aclName v.Resolved
        model, Cmd.none
    | Msg.AclBulkResolveResponse(aclName, Error e) ->
        Core.log e
        eprintfn "Failed to bulk resolve: %A" aclName
        model, Cmd.none

    | Msg.AclRemove aclType ->
        itemAcls.Update(Map.change aclType.Name (fun _ -> None))
        model, Cmd.none

module Renderers =

    type AclDisplayProps = {
        AclParams: Set<AclRefId>
        AclType: AclType
        NavId: NavId
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
                        Some(props.NavId, props.AclType) |> Msg.AclTypeSelection |> dispatch)
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
                    dPrintfn "Render existing with %i{Map.count itemAcls} items"

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
                                                NavId = props.NavId
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
    toGlobalWindow "aclEditor_props" props

    let store, dispatch =
        props |> Store.makeElmish init (update props.Token props.ItemAcls) ignore

    let updateModel () =
        toGlobalWindow "aclEditor_model" store.Value |> ignore

    updateModel ()

    dPrintfn "render AclEditor"

    let makeIAStore (aclType: AclType) =
        // we're trying to map a Map<AclName,Set<_>> to Map<AclName,Set<_> option>
        let getter m : Option<Set<AclRefId>> = m |> Map.tryFind aclType.Name

        let setter (next: Option<Set<AclRefId>>) (oldParent, _) : Map<AclName, Set<AclRefId>> =
            oldParent |> Map.change aclType.Name (fun _ -> next)

        let iaStore: IStore<Set<AclRefId> option> =
            props.ItemAcls
            |> Store.mapStore "ItemAcls" true { Getter = getter; Setter = setter }

        iaStore


    // toggle the acl itself, not a param
    let renderToggleButton aclType store dispatch =
        let iaStore = makeIAStore aclType
        let addText = "Add New Acl"
        let removeText = "Remove"

        let onToggle () =
            printfn "Toggling %A" aclType.Name


            iaStore.Update (function
                | None -> Some Set.empty
                | Some _ -> None)

        Bind.el2 (store |> Store.map (fun v -> v.ResolvedAclStore)) iaStore (fun (itemAcls, pOpt) ->
            printfn "render toggle: %A" pOpt
            let isPresent = Option.isSome pOpt
            let text, icon = if isPresent then removeText, "Remove" else addText, "Add"
            // shouldn't this grab the old params if it was previously set to remove, thereby restoring it

            // printfn "onClickValue: %A" onClickValue

            tButton "Toggle Acl" None ButtonType.Submit [
                data_ "button-purpose" text
                tryIcon (IconSearchType.MuiIcon icon)
                onClick (fun _ -> onToggle ()) []
            ])

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
            text "Acl Type"
            formFieldAddons [] [
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
                                (Option.map (fun aclType -> props.NavId, aclType)
                                 >> Msg.AclTypeSelection
                                 >> dispatch)
                            )
                    }
                    [ data_ "purpose" "aclTypeSelector" ]
                Bind.el (
                    store |> Store.map (MLens.getFocusedAclType) |> Observable.distinctUntilChanged,
                    fun aclTypeOpt ->
                        match aclTypeOpt with
                        | None -> Html.div []
                        | Some aclType -> renderToggleButton aclType store dispatch
                )
            ] []

            // acl editor/creator
            Bind.el (
                store |> Store.map (MLens.getFocusedAclType) |> Observable.distinctUntilChanged,
                function
                | None -> Html.div []
                | Some aclType ->

                    let iaStore: IStore<Set<AclRefId> option> = makeIAStore aclType

                    renderAclTypeEditor {
                        AclType = aclType
                        AclParams = iaStore
                        Token = props.Token
                    }
            )
        ]
    ]
    |> Styling.withCss
