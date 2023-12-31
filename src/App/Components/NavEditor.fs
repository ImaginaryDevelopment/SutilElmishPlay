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

open App.Components.Gen
open App.Components.Gen.Icons
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



[<RequireQualifiedAccess>]
type StandaloneParentMsg =
    | ParentMsg of NavShared.ParentMsg
    | Cancel
    | Saved of NavItem

[<RequireQualifiedAccess>]
type ChildParentMsg =
    | ParentMsg of NavShared.ParentMsg
    | GotFocus

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

type EditorMsgType =
    | EditProp of prop: string * nextValue: string
    | EnabledChange of bool
    | EditAcl of AclEditor.AclParentMsg
    | TabChange of EditorTabs
    | IconMsg of IconEditor.IconEditorParentMsg
    | Save of SaveMsg

module Commands =

    let saveItem token item : Cmd<EditorMsgType> =
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

module Renderers =

    let renderPropsEditor fError value dispatch =
        let ff name value =
            formField [ text name ] [ textInput name value [] (fun v -> EditProp(name, v)) dispatch ]

        fun () ->
            Html.div [
                data_ "method" "renderPropsEditor"
                formField [ text (nameof value.Enabled) ] [
                    checkbox (nameof value.Enabled) value.Enabled [] EnabledChange dispatch
                ]
                <| fError (Some "Enabled")
                ff (nameof value.Weight) (string value.Weight) <| fError (Some "Weight")
                if value.Type = NavItemType.Link then
                    ff (nameof value.Url) value.Url <| fError (Some "Url")
            ]


    let renderFramed lDispatch pDispatch =
        let siblings =

            match pDispatch with
            | EditorParentDispatchType.Standalone pDispatch -> [
                Html.divc "panel-block" [
                    data_ "method" "renderFramed"

                    Html.divc "left-left" [
                        bButton "Save" [
                            text "Save"
                            onClick (fun _ -> EditorMsgType.Save SaveMsg.Requested |> lDispatch) []
                        ]
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

let init (stateStore: LocalStorage.IAccessor<CachedState>) item =
    let initialTab = stateStore.TryGetValue() |> Option.map (fun cs -> cs.Tab) // |> Option.bind (fun cs -> cs.Tab |> EditorTabs.ReParse)

    justModel {
        Tab = initialTab |> Option.defaultValue EditorTabs.IconTab
        Item = item
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
                let f k (acl, v) : NavAclInquiry = {
                    AclName = k
                    NavId = NavId v
                    AclType = acl
                }

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

let update
    (cState, appMode, aclTypes)
    (dispatchParent: EditorParentDispatchType)
    msg
    (model: Model)
    : Model * Cmd<EditorMsgType> =
    printfn "NavEditor update: %A" msg

    let block msg : Model * Cmd<EditorMsgType> =
        model |> MLens.addError msg |> justModel<EditorMsgType>

    match msg with
    // blocks

    | EditAcl(AclEditor.AclParentMsg.CreateAcl(aclRef, _)) when
        model.Item.AclRefs.Keys |> Seq.exists (fun e -> e = aclRef.Name)
        ->
        block $"Acl Create for existing acl '{AclName.getText aclRef.Name}'"

    // actions
    | EditAcl(AclEditor.AclResolveRequest req) ->
        EditorParentDispatchType.DispatchParent dispatchParent (NavShared.ParentMsg.AclParamResolveRequest req)
        justModel model

    | TabChange t ->
        let next = { model with Tab = t }

        justModel
        <| match SideEffects.saveStateCache cState next with
           | Ok() -> next
           | Error e -> MLens.addError e model

    | EnabledChange value ->
        justModel {
            model with
                Item = { model.Item with Enabled = value }
        }
    | IconMsg(IconEditor.IconEditorParentMsg.NameChange(name, value))
    | EditProp(name, value) ->
        try
            let nextItem = cloneSet model.Item name value
            justModel { model with Item = nextItem }
        with ex ->
            Core.log ex
            eprintfn $"%s{ex.Message}"

            MLens.addError $"Failed to set property: %s{name} on %s{serialize model.Item}" model
            |> justModel

    | IconMsg(IconEditor.IconEditorParentMsg.GotFocus) ->
        EditorParentDispatchType.DispatchChildOnly dispatchParent ChildParentMsg.GotFocus
        justModel model

    | EditAcl(AclEditor.AclParentMsg.TypeChange v) ->
        EditorParentDispatchType.DispatchParent dispatchParent (NavShared.ParentMsg.AclTypeChange v)
        justModel model

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

            SideEffects.getUnresolved aclTypes resolvedAcls nextItem dispatchParent

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
    | EditAcl(AclEditor.AclParentMsg.SearchRequest v) ->
        EditorParentDispatchType.DispatchParent dispatchParent (NavShared.ParentMsg.AclSearchRequest v)
        justModel model

    | Save(Responded(Error e)) -> MLens.addError e model |> justModel

    | Save(Responded(Ok item)) ->
        EditorParentDispatchType.DispatchSOOnly dispatchParent
        <| StandaloneParentMsg.Saved item

        justModel model

    // this is save requested, not resolved
    | Save SaveMsg.Requested ->
        match appMode with
        | Auth token -> model, Commands.saveItem token model.Item
        | Demo -> MLens.addError "Save Not implemented in demo" model |> justModel

()

type NavEditorCoreProps = {
    AppMode: ConfigType<string>
    AclTypes: AclType seq
    NavItem: NavItem
    EditorMode: EditorMode
    IsFocus: bool
}

type NavEditorProps = {
    Core: NavEditorCoreProps
    NavItemIconObservable: System.IObservable<string>
}

// renames will go a different route, no path editing
let renderEditor (props: NavEditorProps) =
    printfn "Render editor: %A" props.Core.NavItem.Type
    let cState = getCacheStore props.Core.EditorMode

    match props.Core.EditorMode with
    | EditorMode.Standalone _ -> toGlobalWindow $"navEditor_props" props
    | EditorMode.Child(name, _, _) -> toGlobalWindow $"navEditor_{name}_props" props

    let dispatchParent =
        match props.Core.EditorMode with
        | EditorMode.Standalone dp -> EditorParentDispatchType.Standalone dp
        | EditorMode.Child(_, _, dp) -> EditorParentDispatchType.Child dp

    let store, dispatch =
        props.Core.NavItem
        |> Store.makeElmish
            (init cState)
            (update (cState, props.Core.AppMode, props.Core.AclTypes) dispatchParent)
            ignore

    match props.Core.EditorMode with
    | EditorMode.Standalone _ -> toGlobalWindow "navEditor_model" store.Value
    | EditorMode.Child(name, _, _) -> toGlobalWindow $"navEditor_{name}_model" store.Value

    let vItem, vErrors =
        match props.Core.EditorMode with
        | EditorMode.Standalone _ -> ValidNavItem.ValidateNavItem props.Core.NavItem
        | EditorMode.Child(_, vItem, _) -> vItem
        |> Option.ofResult

    let getError =
        NavShared.renderErrorMapMaybe vErrors (function
            | None -> ""
            | Some v -> v)

    let core =
        let obsTab = store |> Store.map MLens.getTab
        let obsItem = store |> Store.map MLens.getItem

        Bind.el2 obsTab obsItem (fun (tab, value) ->
            renderTabs
                [ "fill" ]
                [
                    {
                        Name = "Props"
                        TabType = Enabled <| TabChange EditorTabs.MainTab
                        IsActive = tab = EditorTabs.MainTab
                        Render = Renderers.renderPropsEditor getError value dispatch

                    }
                    {
                        Name = "Icon"
                        TabType = Enabled <| TabChange EditorTabs.IconTab
                        IsActive = tab = EditorTabs.IconTab
                        Render =
                            fun () ->
                                IconEditor.renderIconEditor
                                    {
                                        PropName = "Icon"
                                        PropObserver =
                                            props.NavItemIconObservable |> Observable.filter (String.isValueString)
                                        PropValue = value.Icon
                                        IsFocus = props.Core.IsFocus
                                    }
                                    (IconMsg >> dispatch)
                    }
                    {
                        Name = "Acls"
                        TabType = Enabled <| TabChange EditorTabs.AclTab
                        IsActive = tab = EditorTabs.AclTab
                        Render =
                            fun () ->
                                AclEditor.renderAclsEditor {
                                    ItemAcls =
                                        value.AclRefs
                                        |> Map.toSeq
                                        |> Seq.map (fun (k, v) -> { Name = k; Parameters = v })
                                    AclTypes = props.Core.AclTypes
                                    DispatchParent =
                                        (fun msg ->
                                            printfn "AclEditor ParentMsg:%A" msg
                                            msg)
                                        >> EditAcl
                                        >> dispatch
                                }
                    }
                ]
                dispatch)

    Html.div [
        disposeOnUnmount [ store ]
        data_ "file" "NavEditor"
        Bind.el (
            store |> Store.map MLens.getItem,
            fun value ->
                match props.Core.EditorMode with
                | Standalone _ ->
                    let siblings = Renderers.renderFramed dispatch dispatchParent

                    App.Components.NavShared.renderEditorFrame value [ core ] siblings
                | Child _ -> core
        )
    ]
