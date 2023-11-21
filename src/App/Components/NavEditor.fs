[<RequireQualifiedAccess>]
module App.Components.NavEditor

open BReusable
open Core

open Sutil
open Sutil.CoreElements

open App.Adapters
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen
open App.Components.Gen.Icons
open App.Adapters.Config

// [<Fable.Core.Erase>]
[<Fable.Core.StringEnum>]
type EditorTabs =
    | IconTab
    | AclTab

type NavEditorErrorType = string

type Model = {
    Tab: EditorTabs
    Item: NavItem
    Errors: (NavEditorErrorType * System.DateTime) list
}

type CachedState = { Tab: EditorTabs }


type ParentMsg =
    | AclTypeChange of Acl
    | AclSearchRequest of AclRefValueArgs

[<RequireQualifiedAccess>]
type StandaloneParentMsg =
    | ParentMsg of ParentMsg
    | Cancel
    | Saved of NavItem

[<RequireQualifiedAccess>]
type ChildParentMsg =
    | ParentMsg of ParentMsg
    | GotFocus

type EditorMode =
    | Standalone of Dispatch<StandaloneParentMsg>
    | Child of name: string * Dispatch<ChildParentMsg>

let getCacheStore em : LocalStorage.IAccessor<CachedState> =
    match em with
    | Standalone _ -> LocalStorage.StorageAccess("NavEditor_CachedState")
    | Child(name, _) -> LocalStorage.StorageAccess($"NavEditor_{name}_CachedState")


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
    | EditAcl of AclEditor.AclParentMsg
    | TabChange of EditorTabs
    | IconMsg of IconEditor.IconEditorMsg
    | Save of SaveMsg

module Commands =

    let saveItem token item : Cmd<EditorMsgType> =
        let mapError (ex: exn) = $"Save Error: {ex.Message}"

        let f item =
            async {
                let! resp = Api.NavItems.save token item

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
    let renderIconEditor
        (propName, propObs)
        isFocus
        (value: string)
        (dispatch: Dispatch<EditorMsgType>)
        (pDispatch: EditorParentDispatchType)
        =
        let dispatch2 =
            function
            | App.Components.IconEditor.IconEditorMsg.NameChange(propName, value) ->
                dispatch (EditProp(propName, value))
            | IconEditor.IconEditorMsg.GotFocus ->
                EditorParentDispatchType.DispatchChildOnly pDispatch ChildParentMsg.GotFocus

        App.Components.IconEditor.renderIconEditor
            {
                PropName = propName
                PropObserver = propObs
                PropValue = value
                IsFocus = isFocus
            }
            dispatch2

    let renderEditorFrame
        (value: NavItem)
        core
        (lDispatch: Dispatch<EditorMsgType>)
        (pDispatch: EditorParentDispatchType)
        =
        Html.divc "panel" [
            data_ "method" "renderEditorFrame"
            // path
            Html.pc "panel-heading" [
                if value.Type = Folder then
                    tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
                Html.span [ text $"{value.Name}: {value.Path}" ]
            ]

            Html.divc "panel-block" core

            match pDispatch with
            | EditorParentDispatchType.Standalone pDispatch ->
                Html.divc "panel-block" [

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
            | _ -> ()
            Html.pre [ text (Core.pretty value) ]
        ]

let justModel<'tMsg> m : Model * Cmd<'tMsg> = m, Cmd.none

let init (stateStore: LocalStorage.IAccessor<CachedState>) item =
    let initialTab = stateStore.TryGetValue() |> Option.map (fun cs -> cs.Tab) // |> Option.bind (fun cs -> cs.Tab |> EditorTabs.ReParse)

    justModel {
        Tab = initialTab |> Option.defaultValue IconTab
        Item = item
        Errors = List.empty
    }

module SideEffects =

    let saveStateCache (stateStore: LocalStorage.IAccessor<CachedState>) (next: Model) =
        Some { Tab = next.Tab } |> stateStore.TrySetValue

let update
    (cState, appMode)
    (dispatchParent: EditorParentDispatchType)
    msg
    (model: Model)
    : Model * Cmd<EditorMsgType> =
    printfn "NavEditor update: %A" msg

    let block msg : Model * Cmd<EditorMsgType> =
        model |> MLens.addError msg |> justModel<EditorMsgType>

    match msg with
    // blocks

    | EditAcl(AclEditor.AclParentMsg.CreateAcl(aclRef, acl)) when
        model.Item.AclRefs |> Seq.exists (fun e -> e.Name = acl.Name)
        ->
        block $"Acl Create for existing acl '{acl.Name}'"

    // actions

    | TabChange t ->
        let next = { model with Tab = t }

        justModel
        <| match SideEffects.saveStateCache cState next with
           | Ok() -> next
           | Error e -> MLens.addError e model

    | IconMsg(IconEditor.IconEditorMsg.NameChange(name, value))
    | EditProp(name, value) ->
        let nextItem = cloneSet model.Item name value
        justModel { model with Item = nextItem }

    | IconMsg(IconEditor.IconEditorMsg.GotFocus) ->
        EditorParentDispatchType.DispatchChildOnly dispatchParent ChildParentMsg.GotFocus
        justModel model

    | EditAcl(AclEditor.AclParentMsg.TypeChange v) ->
        EditorParentDispatchType.DispatchParent dispatchParent (AclTypeChange v)
        justModel model

    | EditAcl(AclEditor.AclParentMsg.Change(aclName, pt, aclValue)) ->
        let nextItem =
            cloneSet
                model.Item
                (nameof model.Item.AclRefs)
                (model.Item.AclRefs
                 |> Seq.map (fun v ->
                     if v.Name = aclName then
                         {
                             v with
                                 Parameters =
                                     match pt with
                                     | AclEditor.AclParamModificationType.AddParam ->
                                         v.Parameters |> Set.ofArray |> Set.add aclValue |> Set.toArray
                                     | AclEditor.AclParamModificationType.RemoveParam ->
                                         v.Parameters |> Seq.filter (fun pv -> pv <> aclValue) |> Array.ofSeq
                         }
                     else
                         v)
                 |> Array.ofSeq)

        justModel { model with Item = nextItem }

    | EditAcl(AclEditor.Remove acl) ->
        let nextItem =
            cloneSet
                model.Item
                (nameof model.Item.AclRefs)
                (model.Item.AclRefs
                 |> Seq.filter (fun aclRef -> aclRef.Name <> acl.Name)
                 |> Array.ofSeq)

        justModel { model with Item = nextItem }

    | EditAcl(AclEditor.AclParentMsg.CreateAcl(aclRef, acl)) ->
        printfn "cloning item to add acl"
        // should we try to block repeated acl names?
        let nextItem =
            let n = nameof model.Item.AclRefs
            cloneSet model.Item n (model.Item.AclRefs |> Seq.append [ aclRef ] |> Array.ofSeq)

        justModel { model with Item = nextItem }
    | EditAcl(AclEditor.AclParentMsg.SearchRequest v) ->
        EditorParentDispatchType.DispatchParent dispatchParent (ParentMsg.AclSearchRequest v)
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
    AclTypes: Acl seq
    AclSearchResponse: AclSearchResponse option
    NavItem: NavItem
    EditorMode: EditorMode
    IsFocus: bool
}

type NavEditorProps = {
    Core: NavEditorCoreProps
    ResolvedAclParams: System.IObservable<Map<string, AclDisplay>>
    NavItemIconObservable: System.IObservable<string>
}

// renames will go a different route, no path editing
let renderEditor (props: NavEditorProps) =
    let cState = getCacheStore props.Core.EditorMode

    match props.Core.EditorMode with
    | EditorMode.Standalone _ -> toGlobalWindow $"navEditor_props" props
    | EditorMode.Child(name, _) -> toGlobalWindow $"navEditor_{name}_props" props

    let dispatchParent =
        match props.Core.EditorMode with
        | EditorMode.Standalone dp -> EditorParentDispatchType.Standalone dp
        | EditorMode.Child(_, dp) -> EditorParentDispatchType.Child dp

    let store, dispatch =
        props.Core.NavItem
        |> Store.makeElmish (init cState) (update (cState, props.Core.AppMode) dispatchParent) ignore

    match props.Core.EditorMode with
    | EditorMode.Standalone _ -> toGlobalWindow "navEditor_model" store.Value
    | EditorMode.Child(name, _) -> toGlobalWindow $"navEditor_{name}_model" store.Value

    let core =
        let obsTab = store |> Store.map MLens.getTab
        let obsItem = store |> Store.map MLens.getItem

        Bind.el2 obsTab obsItem (fun (tab, value) ->
            renderTabs
                "fill"
                [
                    {
                        Name = "Icon"
                        TabClickMsg = TabChange IconTab
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
                                    (IconMsg >> dispatch) // viewNavRoot (store,dispatch)
                    }
                    {
                        Name = "Acls"
                        TabClickMsg = TabChange AclTab
                        IsActive = tab = AclTab
                        Render =
                            fun () ->
                                AclEditor.renderAclsEditor {
                                    ItemAclRefs = value.AclRefs
                                    AclTypes = props.Core.AclTypes
                                    ResolvedParams = props.ResolvedAclParams
                                    AclSearchResponse = props.Core.AclSearchResponse
                                    DispatchParent = EditAcl >> dispatch
                                }
                    }
                ]
                dispatch)

    Html.div [
        disposeOnUnmount [ store ]
        Bind.el (
            store |> Store.map MLens.getItem,
            fun value ->
                Renderers.renderEditorFrame
                    value
                    [
                        core
                    // oldCore
                    ]
                    dispatch
                    dispatchParent
        )
    ]
