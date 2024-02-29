[<RequireQualifiedAccess>]
module App.Components.NavEditor

open BReusable
open Core

open Sutil
open Sutil.CoreElements

open App.Adapters
open App.Adapters.Schema
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api
open App.Adapters.Api.Shared
open App.Adapters.Api.Schema
open App.Adapters.Api.Mapped

open App.Adapters.Config

// [<Fable.Core.Erase>]
[<Fable.Core.StringEnum; RequireQualifiedAccess>]
type EditorTabs =
    | MainTab
    | IconTab
    | AclTab

type NavEditorErrorType = string

type Model = {
    Tab: EditorTabs
    Item: NavItem
    Errors: (NavEditorErrorType * System.DateTime) list
}

type CachedState = { Tab: EditorTabs }

// Messages we can send to our parent when we aren't a NavCreator child
[<RequireQualifiedAccess>]
type StandaloneParentMsg =
    | ParentMsg of NavShared.ParentMsg
    | Cancel
    | Saved of NavItem

// Messages we can send to NavCreator when we are a child
[<RequireQualifiedAccess>]
type ChildParentMsg =
    | ParentMsg of NavShared.ParentMsg
    | ItemUpdate of NavItem
// | GotFocus

type EditorMode =
    | Standalone of Dispatch<StandaloneParentMsg>
    | Child of name: string * validation: NavValidation * Dispatch<ChildParentMsg>

let getCacheStore em : LocalStorage.IAccessor<CachedState> =
    match em with
    | Standalone _ -> LocalStorage.StorageAccess("NavEditor_CachedState")
    | Child(name, _, _) -> LocalStorage.StorageAccess($"NavEditor_{name}_CachedState")


[<RequireQualifiedAccess>]
module private MLens =
    let getTab (x: Model) = x.Tab
    let getItem x = x.Item

    let addError msg (x: Model) = {
        x with
            Errors = (msg, System.DateTime.Now) :: x.Errors
    }

type SaveMsg =
    | Requested
    | Responded of Result<NavItem, NavEditorErrorType>


type private Msg =
    | EditProp of prop: string * nextValue: string
    | EnabledChange of bool
    | EditAcl of AclEditor.AclParentMsg
    | TabChange of EditorTabs
    | IconMsg of IconEditor.IconEditorParentMsg
    | Save of SaveMsg

module private Commands =

    let saveItem token item : Cmd<Msg> =
        let mapError (ex: exn) = $"Save Error: {ex.Message}"

        let f item =
            async {
                let! resp = NavItems.save token item

                // TODO: swap item with response item, or at least compare them?
                return
                    Save
                    <| Responded(resp |> Result.map (fun _ -> item) |> Result.mapError mapError)
            }

        Cmd.OfAsync.either f item id (mapError >> Error >> Responded >> Save)

[<RequireQualifiedAccess>]
type EditorParentDispatchType =
    | Standalone of Dispatch<StandaloneParentMsg>
    | Child of Dispatch<ChildParentMsg>

    static member DispatchParent pdt msg =
        match pdt with
        | Standalone pDispatch -> pDispatch (StandaloneParentMsg.ParentMsg msg)
        | Child pDispatch -> pDispatch (ChildParentMsg.ParentMsg msg)

    static member DispatchChildOnly pdt msg =
        match pdt with
        | Child pDispatch -> pDispatch msg
        | _ -> ()

    static member DispatchSOOnly pdt msg =
        match pdt with
        | Standalone pDispatch -> pDispatch msg
        | _ -> ()

module private Renderers =

    let renderPropsEditor fError (store: IReadOnlyStore<NavItem>) dispatch =
        let enabledStore =
            store
            |> Store.mapRStore
                {
                    UseEquality = true
                    DebugTitle = None
                }
                (fun navItem -> navItem.Enabled)

        let weightStore = store |> Store.map (fun v -> string v.Weight)
        let urlStore = store |> Store.map (fun v -> v.Url)
        printfn "Render PropsEditor"
        let value = store.Value

        let ff name value =
            formField [ text name ] [
                textInput
                    {
                        Titling = name
                        Value = value
                        OnChange = fun v -> EditProp(name, v) |> dispatch
                        DebounceOverride = None
                    }
                    []
            ]


        fun () ->
            // printfn "Focus is %A" propsFocusOpt
            Html.div [
                let enabledField =
                    formField [ text (nameof value.Enabled) ] [
                        checkbox (nameof value.Enabled) enabledStore [] EnabledChange dispatch
                    ]

                let weightField = ff (nameof value.Weight) weightStore

                data_ "method" "renderPropsEditor"
                enabledField <| fError (Some "Enabled")

                weightField <| fError (Some "Weight")

                if value.Type = NavItemType.Link then
                    let urlField = ff (nameof value.Url) urlStore
                    urlField <| fError (Some "Url")
            ]

    let renderFramed lDispatch pDispatch =
        let siblings =

            match pDispatch with
            | EditorParentDispatchType.Standalone pDispatch -> [
                Html.divc "panel-block" [
                    data_ "method" "renderFramed"

                    Html.divc "left-left" [
                        bButton "Save" [ text "Save"; onClick (fun _ -> Msg.Save SaveMsg.Requested |> lDispatch) [] ]
                        rButton "Cancel" [
                            text "Cancel"
                            onClick (fun _ -> StandaloneParentMsg.Cancel |> pDispatch) []
                        ]
                    ]
                ]
              ]
            | _ -> List.empty

        siblings

let justModel<'tMsg> m : Model * Cmd<'tMsg> = m, Cmd.none

// let init (stateStore: LocalStorage.IAccessor<CachedState>) item =
let init (item: IReadOnlyStore<NavItem>) =
    printfn "NavEditor init"
    // let initialTab = stateStore.TryGetValue() |> Option.map (fun cs -> cs.Tab) // |> Option.bind (fun cs -> cs.Tab |> EditorTabs.ReParse)

    justModel {
        Tab = EditorTabs.IconTab
        Item = item.Value
        Errors = List.empty
    }

module SideEffects =

    let saveStateCache (stateStore: LocalStorage.IAccessor<CachedState>) (next: Model) =
        Some { Tab = next.Tab } |> stateStore.TrySetValue

    let getUnresolved (aclTypes: AclType seq) (resolvedAcls: ResolvedAclLookup) navItem dispatchParent =
        let unresolvedAcls =
            navItem
            |> NavItem.GetRefParams aclTypes
            |> Map.choose (fun k (aclType, v) ->
                match resolvedAcls |> Map.tryFind k with
                | None -> // all params need resolved
                    Some(k, (aclType, v))
                | Some ra ->
                    let unresolved = ra.Keys |> Set.ofSeq |> Set.difference v
                    Some(k, (aclType, unresolved)))

        if not <| Map.isEmpty unresolvedAcls then
            let aclParamResolveRequests =

                unresolvedAcls
                |> Map.toSeq
                |> Seq.collect (fun (k, (aclType, v)) ->
                    v
                    |> Seq.map (fun v ->
                        let req: AclRefLookup = { // NavAclInquiry
                            AclName = k
                            AclRefId = v
                            AclType = aclType.AclParamType
                        }

                        req))
                |> Seq.map NavShared.ParentMsg.AclParamResolveRequest
                |> List.ofSeq

            match dispatchParent with
            | EditorParentDispatchType.Child dispatchParent ->
                aclParamResolveRequests
                |> List.map ChildParentMsg.ParentMsg
                |> List.iter dispatchParent
            | EditorParentDispatchType.Standalone dispatchParent ->
                aclParamResolveRequests
                |> List.map StandaloneParentMsg.ParentMsg
                |> List.iter dispatchParent

let private update
    (appMode, aclTypes: IReadOnlyStore<_>)
    (dispatchParent: EditorParentDispatchType)
    msg
    (model: Model)
    : Model * Cmd<Msg> =
    printfn "NavEditor update: %A" msg

    let block msg : Model * Cmd<Msg> =
        model |> MLens.addError msg |> justModel<Msg>

    match msg with
    // blocks

    | EditAcl(AclEditor.AclParentMsg.CreateAcl(aclRef, _)) when
        model.Item.AclRefs.Keys |> Seq.exists (fun e -> e = aclRef.Name)
        ->
        block $"Acl Create for existing acl '{AclName.getText aclRef.Name}'"

    // actions
    // | EditAcl(AclEditor.AclResolveRequest req) ->
    //     EditorParentDispatchType.DispatchParent dispatchParent (NavShared.ParentMsg.AclParamResolveRequest req)
    //     justModel model

    | TabChange t ->
        let next = { model with Tab = t }

        // justModel
        // <| match SideEffects.saveStateCache cState next with
        //    | Ok() -> next
        //    | Error e -> MLens.addError e model
        next, Cmd.none

    | EnabledChange value ->
        justModel {
            model with
                Item = { model.Item with Enabled = value }
        }

    | IconMsg(IconEditor.IconEditorParentMsg.Accepted value) ->
        let name = "Icon"

        try
            let nextItem = cloneSet model.Item name value
            Core.log ("EditProp", name, value, nextItem)
            EditorParentDispatchType.DispatchChildOnly dispatchParent (ChildParentMsg.ItemUpdate nextItem)
            let nextModel = { model with Item = nextItem }

            nextModel |> justModel
        with ex ->
            Core.log ex
            eprintfn $"%s{ex.Message}"

            MLens.addError $"Failed to set property: %s{name} on %s{serialize model.Item}" model
            |> justModel

    | EditProp(name, value) ->
        try
            let nextItem = cloneSet model.Item name value
            Core.log ("EditProp", name, value, nextItem)
            EditorParentDispatchType.DispatchChildOnly dispatchParent (ChildParentMsg.ItemUpdate nextItem)
            let nextModel = { model with Item = nextItem }

            nextModel |> justModel
        with ex ->
            Core.log ex
            eprintfn $"%s{ex.Message}"

            MLens.addError $"Failed to set property: %s{name} on %s{serialize model.Item}" model
            |> justModel

    // | EditAcl(AclEditor.AclParentMsg.TypeChange v) ->
    //     EditorParentDispatchType.DispatchParent dispatchParent (NavShared.ParentMsg.AclTypeChange v)
    //     justModel model

    | EditAcl(AclEditor.AclParentMsg.Change(aclName, pt, aclValue)) ->

        printfn "Edited acl"

        let nextItem =
            match pt with
            | AclEditor.AclParamModificationType.AddParam -> NavItem.AddAclRefParam aclName aclValue model.Item
            | AclEditor.AclParamModificationType.RemoveParam ->
                NavItem.TryChangeAclNameParam aclName (Set.remove aclValue) model.Item

        match nextItem with
        | Ok nextItem ->
            // TODO: check for map not having resolved the acls, how do we make sure that's not already in flight?
            // is the map AclName -> AclDisplay or AclDisplay guid to AclDisplay?
            let resolvedAcls = App.Global.resolvedAclLookup.Value

            SideEffects.getUnresolved aclTypes.Value resolvedAcls nextItem dispatchParent

            let nextModel = { model with Item = nextItem }

            justModel nextModel
        | Error e -> MLens.addError e model, Cmd.none

    | EditAcl(AclEditor.Remove acl) ->
        let nextItem = {
            model.Item with
                AclRefs = model.Item.AclRefs.Remove acl.Name
        }

        justModel { model with Item = nextItem }

    | EditAcl(AclEditor.AclParentMsg.CreateAcl(aclRef, acl)) ->
        printfn "cloning item to add acl"
        // should we try to block repeated acl names?
        let nextItem = {
            model.Item with
                AclRefs = model.Item.AclRefs |> Map.add aclRef.Name acl
        }

        justModel { model with Item = nextItem }
    // | EditAcl(AclEditor.AclParentMsg.SearchRequest v) ->
    //     EditorParentDispatchType.DispatchParent dispatchParent (NavShared.ParentMsg.AclSearchRequest v)
    //     justModel model

    | Save(Responded(Error e)) ->
        eprintfn "NavEditor Api Error: %A" e
        MLens.addError e model |> justModel

    | Save(Responded(Ok item)) ->
        EditorParentDispatchType.DispatchSOOnly dispatchParent
        <| StandaloneParentMsg.Saved item

        justModel model

    // this is save requested, not resolved
    | Save SaveMsg.Requested ->
        match appMode with
        | Auth token -> model, Commands.saveItem token model.Item
        | Demo -> MLens.addError "Save not implemented in demo" model |> justModel

()

type NavEditorProps = {
    AppMode: ConfigType<string>
    AclTypes: IReadOnlyStore<AclType seq>
    NavItem: IStore<NavItem>
    EditorMode: EditorMode
}

// renames will go a different route, no path editing
let renderEditor (props: NavEditorProps) =
    printfn "Render editor: %A" props.NavItem.Value.Type
    // let cState = getCacheStore props.Core.EditorMode

    match props.EditorMode with
    | EditorMode.Standalone _ -> toGlobalWindow $"navEditor_props" props
    | EditorMode.Child(name, _, _) -> toGlobalWindow $"navEditor_{name}_props" props

    let dispatchParent =
        match props.EditorMode with
        | EditorMode.Standalone dp -> EditorParentDispatchType.Standalone dp
        | EditorMode.Child(_, _, dp) -> EditorParentDispatchType.Child dp

    let store, dispatch =
        props.NavItem
        |> Store.makeElmish init (update (props.AppMode, props.AclTypes) dispatchParent) ignore

    let obsTab =
        store
        |> Store.mapRStore
            {
                UseEquality = true
                DebugTitle = None
            }
            MLens.getTab

    let obsItem =
        store
        |> Store.mapStore "obsItem" true (MLens.getItem, (fun nextItem -> { store.Value with Item = nextItem }))

    match props.EditorMode with
    | EditorMode.Standalone _ -> toGlobalWindow "navEditor_model" store.Value
    | EditorMode.Child(name, _, _) -> toGlobalWindow $"navEditor_{name}_model" store.Value

    let vItem, vErrors =
        match props.EditorMode with
        | EditorMode.Standalone _ -> ValidNavItem.ValidateNavItem props.NavItem.Value
        | EditorMode.Child(_, vItem, _) -> vItem
        |> Option.ofResult

    let getError =
        NavShared.renderErrorMapMaybe vErrors (function
            | None -> ""
            | Some v -> v)

    let propsTab = Renderers.renderPropsEditor getError obsItem dispatch ()

    let core =

        printfn "Render NavEditor tabs"

        renderTabs
            [ "fill" ]
            [
                {
                    Name = "Props"
                    TabType = NoVariance(Enabled <| TabChange EditorTabs.MainTab)
                    IsActive =
                        let x =
                            obsTab
                            |> Store.mapRStore
                                {
                                    UseEquality = true
                                    DebugTitle = None
                                }
                                (fun tab -> tab = EditorTabs.MainTab)

                        x
                    Value = propsTab
                }
                {
                    Name = "Icon"
                    TabType = NoVariance(Enabled <| TabChange EditorTabs.IconTab)
                    IsActive =
                        obsTab
                        |> Store.mapRStore
                            {
                                UseEquality = true
                                DebugTitle = None
                            }
                            (fun tab -> tab = EditorTabs.IconTab)
                    Value = IconEditor.renderIconEditor { PropValue = store.Value.Item.Icon } (IconMsg >> dispatch)
                }
                {
                    Name = "Acls"
                    TabType = NoVariance(Enabled <| TabChange EditorTabs.AclTab)
                    IsActive =
                        obsTab
                        |> Store.mapRStore
                            {
                                UseEquality = true
                                DebugTitle = None
                            }
                            (fun tab -> tab = EditorTabs.AclTab)
                    Value =
                        Bind.el (
                            props.AclTypes,
                            fun aclTypes ->
                                AclEditor.renderAclsEditor {
                                    ItemAcls =
                                        store.Value.Item.AclRefs
                                        |> Map.toSeq
                                        |> Seq.map (fun (k, v) -> { Name = k; Parameters = v })
                                        |> List.ofSeq
                                    AclTypes = aclTypes
                                    AclLookupStore = None
                                    DispatchParent =
                                        (fun msg ->
                                            printfn "AclEditor ParentMsg:%A" msg
                                            msg)
                                        >> EditAcl
                                        >> dispatch
                                }
                        )
                }
            ]
            dispatch

    Html.div [
        disposeOnUnmount [ store ]
        data_ "file" "NavEditor"
        printfn "RenderNavEditor part"

        match props.EditorMode with
        | Standalone _ ->
            let siblings = Renderers.renderFramed dispatch dispatchParent

            Bind.el (
                store |> Store.map MLens.getItem,
                fun item -> App.Components.NavShared.renderEditorFrame item [ core ] siblings
            )
        | Child _ -> core
    ]
