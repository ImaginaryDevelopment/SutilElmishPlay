module App.Components.Admin.AclTypeEditor

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

type Model = {
    ResolvedAclStore: IStore<ResolvedAclLookup>
    SearchIsInFlight: bool
    SearchResults: AclDisplay list
    // should the date time be optional for only expiring errors?
    ErrorQueue: (string * System.DateTime) list
}

type private Msg =
    | AclSearchRequest of AclRefValueArgs
    | AclSearchResponse of Result<AclSearchResult, ErrorType>
    | AclParamSelection of AclRefId

module private Commands =
    // HACK: this does not properly register with global as an inflight deal, nor check if one is already in flight
    // and neither did any other code in this app apparently
    let searchAclRefValues token req : Cmd<Msg> =
        let f x =
            async {
                let! resp = Api.Shared.searchAclRefValues token x

                match resp with
                | Ok v -> return Ok v
                | Error e -> return Error(Choice2Of2 e)
            }

        Cmd.OfAsync.either f req id (fun ex -> Choice2Of2 ex |> Error)
        |> Cmd.map Msg.AclSearchResponse

let private update token (aclParams: IStore<Set<AclRefId> option>) msg (model: Model) : Model * Cmd<Msg> =
    printfn "AclTypeEditor update: %A"
    <| BReusable.String.truncateDisplay false 200 (string msg)

    match msg with
    | Msg.AclParamSelection(aclRefId) ->
        aclParams.Update (function
            | None -> Some(Set.singleton aclRefId)
            | Some p ->
                if p |> Set.contains aclRefId then
                    if Set.count p = 1 then
                        None
                    else
                        p |> Set.remove aclRefId |> Some
                else
                    p |> Set.add aclRefId |> Some)

        model, Cmd.none

    | Msg.AclSearchRequest x ->
        let cmd = Commands.searchAclRefValues token x
        { model with SearchIsInFlight = true }, cmd

    | Msg.AclSearchResponse(Error e) ->
        eprintfn "AclSearchResponse: %A" e
        { model with SearchIsInFlight = false }, Cmd.none

    | Msg.AclSearchResponse(Ok aclSearchResult) ->
        aclSearchResult.Data.Results
        |> Seq.map (fun ad -> ad.Reference, ad)
        |> App.Global.ResolvedAclLookup.addValues aclSearchResult.AclName

        aclSearchResult.Data
        |> fun x -> (x.Search, x.Results |> List.ofArray)
        |> App.Global.AclSearchResponse.addValue aclSearchResult.AclName

        {
            model with
                SearchIsInFlight = false
                SearchResults = aclSearchResult.Data.Results |> List.ofArray
        },
        Cmd.none

module private Renderers =
    type ReferenceParamsProps = {
        AclType: AclType
        SearchStore: IStore<string>
        SearchResults: IReadOnlyStore<AclDisplay list>
        SearchInFlight: System.IObservable<bool>
        CurrentParamsStore: IReadOnlyStore<Set<AclRefId> option>
    }
    // all reference params at least at this time, are searchable
    let renderReferenceParams (props: ReferenceParamsProps) (dispatch: Msg -> unit) =
        props.CurrentParamsStore.Value
        |> function
            | None -> printfn "Render ref p with None"
            | Some s -> Set.count s |> printfn "Render ref p with %i"

        Html.div [
            Html.form [

                Bind.el (
                    props.SearchInFlight,
                    fun isInFlight ->
                        formFieldAddons [] [
                            textInput
                                {
                                    Titling = "Search"
                                    Value = props.SearchStore
                                    OnChange = (fun v -> props.SearchStore.Update(fun _ -> v))
                                    DebounceOverride = None
                                }
                                [
                                    if isInFlight then
                                        Attr.disabled true
                                ]
                            tButton "Search" None Submit [
                                // text "Search Icons"
                                tryIcon (IconSearchType.MuiIcon "Search")
                                if isInFlight then
                                    Attr.disabled true
                                else
                                    onClick
                                        (fun _ ->
                                            match props.SearchStore.Value with
                                            | ValueString v ->
                                                Msg.AclSearchRequest {
                                                    AclName = props.AclType.Name
                                                    SearchText = String.trim v
                                                    Max = None
                                                }
                                                |> dispatch
                                            | _ -> eprintfn "Search attempt with no value")
                                        [ EventModifier.PreventDefault ]
                            ]
                        ] []
                )
            ]

            Html.div [
                let renderAclParams
                    (currentParams: Set<AclRefId> option, lookupMap: IReadOnlyStore<Map<AclRefId, AclDisplay>>)
                    (aclRefIdentifier: Choice<AclRefId, AclDisplay>)
                    =
                    currentParams
                    |> Option.defaultValue Set.empty
                    |> Set.count
                    |> printfn "render AclParams-%i params- '%A'"
                    |> fun f -> f aclRefIdentifier

                    let aclRefId =
                        aclRefIdentifier
                        |> function
                            | Choice1Of2 ari -> ari
                            | Choice2Of2 ad -> ad.Reference

                    let isSelected =
                        currentParams |> Option.map (Set.contains aclRefId) |> Option.defaultValue false




                    // let aclDisplayOpt =
                    //     lookupMap |> Map.tryFind aclRefId |> Option.map (fun v -> v.DisplayName)

                    // let aclDisplay =
                    //     aclDisplayOpt |> Option.defaultWith (fun () -> AclRefId.getText aclRefId)

                    Html.a [
                        Attr.title <| AclRefId.getText aclRefId

                        match aclRefIdentifier with
                        | Choice2Of2 ad -> text ad.DisplayName
                        | Choice1Of2 ari ->
                            printfn "Creating deferred display for %A" ari

                            let textObs =
                                lookupMap
                                |> Store.map (fun lm ->
                                    //    printfn "textObs: %i" lm.Count

                                    let next =
                                        lm
                                        |> Map.tryFind ari
                                        |> Option.map (fun v -> v.DisplayName)
                                        |> Option.defaultValue (AclRefId.getText ari)

                                    //    printfn "Value changed: %A - %s" ari next
                                    next)

                            Bind.fragment textObs text

                        Attr.className (if isSelected then "is-active" else "has-text-danger")

                        onClick
                            (fun _ ->
                                // would be nice to map the display here, but eh...
                                printfn "Search item selected: %s" <| AclRefId.getText aclRefId
                                Msg.AclParamSelection(aclRefId) |> dispatch)
                            []
                    ]

                // let ralLookup =
                //     App.Global.resolvedAclLookup
                //     |> Store.map (Map.tryFind aclType.Name >> Option.defaultValue Map.empty)
                let rapRStore =
                    App.Global.resolvedAclLookup
                    |> Store.mapRStore
                        {
                            UseEquality = false
                            DebugTitle = None
                        }
                        (fun v ->
                            let next = v |> Map.tryFind props.AclType.Name |> Option.defaultValue Map.empty
                            // printfn "rapRStore update %i - %A: %i" v.Count props.AclType.Name next.Count
                            next)


                columns2 [] [
                    Html.h2 [ text "Has" ]
                    // selected items
                    Bind.el (
                        props.CurrentParamsStore,
                        fun currentParams ->


                            match currentParams with
                            | None ->
                                // printfn "Render Has with None"
                                Html.div []
                            | Some p ->
                                // Set.count p |> printfn "Render Has with: %i"

                                Html.ul [
                                    // TODO: this needs to be sorted by observable results
                                    for aclRefId in p do
                                        Html.li [ renderAclParams (currentParams, rapRStore) (Choice1Of2 aclRefId) ]
                                ]
                    )

                ] [
                    Html.h2 [ text "Available" ]
                    // HACK: this is looking at all search results, not just current search results
                    // unselected items - based on the lookup of this acl name, or based on the search results? uh oh
                    Bind.el2 props.CurrentParamsStore props.SearchResults (fun (currentParams, searchResults) ->
                        let cp = currentParams |> Option.defaultValue Set.empty
                        let lookupMap = rapRStore

                        Html.ul [
                            for ad in searchResults |> List.filter (fun ad -> cp |> Set.contains ad.Reference |> not) do
                                Html.li [ renderAclParams (currentParams, lookupMap) (Choice2Of2 ad) ]
                        ])

                ]

            ]

        ]

    // items is all possible selectable items
    // does this not account for unchanged items and their original already selected values?
    // there are no select-ables that are not multi right now
    let renderSelectableParams aclType items (store: IReadOnlyStore<Set<AclRefId> option>) dispatch =
        printfn "renderSelectableParams"
        // select for param values to be included
        selectInput
            {
                Values = items |> Seq.map AclRefId
                HasEmpty = not aclType.MultiValue
                ValueGetter = AclRefId.getText
                NameGetter = AclRefId.getText
                OptionChildren = fun child -> []
                SelectType =
                    if aclType.MultiValue then
                        // a list of selected AclRefIds
                        let oStore =
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (Option.map Set.toList >> Option.defaultValue List.empty)

                        ObservedMulti(
                            oStore,
                            (fun ari ->
                                //store |> MLens.updateAclParamStore ari
                                Msg.AclParamSelection(ari) |> dispatch)
                        )
                    else
                        let pStore =
                            store
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (Option.defaultValue Set.empty >> Set.toSeq >> Seq.tryHead)

                        ObservedSelect(
                            pStore,
                            Option.iter <| fun aclRefId -> Msg.AclParamSelection(aclRefId) |> dispatch
                        )

            }
            [ data_ "purpose" "param selector" ]


type AclTypeEditorProps = {
    AclType: AclType

    // None = remove the acl?
    AclParams: IStore<Set<AclRefId> option>
    // was this already in the api, or are we doing an upsert?
    // Existed: bool
    Token: string
}

// todo: probably initiate acl resolve of existing props that aren't already present in lookup
let private init props : Model * Cmd<Msg> =
    {
        ResolvedAclStore = App.Global.resolvedAclLookup
        SearchIsInFlight = false
        SearchResults = List.empty
        // should the date time be optional for only expiring errors?
        ErrorQueue = List.empty
    },
    Cmd.none

let renderAclTypeEditor (props: AclTypeEditorProps) =
    printfn "renderAclInterface"
    let sStore = "" |> Store.make

    let store, dispatch =
        props |> Store.makeElmish init (update props.Token props.AclParams) ignore

    // account for multiples?
    let renderParamSelector (aclType: AclType) =
        // selector if not searchable
        // search and selector if searchable
        printfn "renderParamSelector"

        match aclType.AclParamType with
        | AclParameterType.None -> Html.div [ Attr.title "No Params" ]
        | AclParameterType.Selectable items ->
            // this might need to be the same as the reference one

            Renderers.renderSelectableParams aclType items props.AclParams dispatch
        | AclParameterType.Reference _searchable ->
            let srRStore =
                store
                |> Store.mapRStore
                    {
                        UseEquality = true
                        DebugTitle = None
                    }
                    (fun model -> model.SearchResults)

            let sfObs = store |> Store.map (fun model -> model.SearchIsInFlight)

            Renderers.renderReferenceParams
                {
                    SearchInFlight = sfObs
                    AclType = aclType
                    SearchStore = sStore
                    SearchResults = srRStore
                    CurrentParamsStore = props.AclParams
                }
                dispatch

    Html.div [
        disposeOnUnmount [ store ]
        if props.AclType.AclParamType <> AclParameterType.None then
            Html.div [
                Html.h2 [
                    let titling =
                        // let t = if props.Existed then "Editing" else "Creating"
                        // $"{t} {props.AclType.Name}"
                        $"{props.AclType.Name}"

                    text titling
                ]
                renderParamSelector props.AclType

            ]
        else
            Html.div []
    ]
