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

type CachedState = { Tab: EditorTabs }

let stateStore: LocalStorage.IAccessor<CachedState> =
    LocalStorage.StorageAccess("NavEditor_CachedState")

type Model = {
    Tab: EditorTabs
    Item: NavItem
    Errors: (NavEditorErrorType * System.DateTime) list
}

[<RequireQualifiedAccess>]
module private MLens =
    let getTab x = x.Tab
    let getItem x = x.Item

    let addError msg (x: Model) = {
        x with
            Errors = (msg, System.DateTime.Now) :: x.Errors
    }

type ParentMsg =
    | Cancel
    | Saved of NavItem
    | AclTypeChange of Acl
    | AclSearchRequest of AclRefValueArgs

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
                let! resp = Api.save token item

                // TODO: swap item with response item, or at least compare them?
                return
                    Save
                    <| Responded(resp |> Result.map (fun _ -> item) |> Result.mapError mapError)
            }

        Cmd.OfAsync.either f item id (mapError >> Error >> Responded >> Save)

module Renderers =
    let renderIconEditor (propName, propObs) (value: string) (dispatch: Dispatch<EditorMsgType>) =
        let dispatch2 =
            function
            | App.Components.IconEditor.IconEditorMsg.NameChange(propName, value) ->
                dispatch (EditProp(propName, value))

        App.Components.IconEditor.renderIconEditor
            {
                PropName = propName
                PropObserver = propObs
                PropValue = value
            }
            dispatch2

    let renderEditorFrame (value: NavItem) core (lDispatch: Dispatch<EditorMsgType>) (pDispatch: Dispatch<ParentMsg>) =
        Html.divc "panel" [
            data_ "method" "renderEditorFrame"
            // path
            Html.pc "panel-heading" [
                if value.Type = "Folder" then
                    tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
                Html.span [ text $"{value.Name}: {value.Path}" ]
            ]

            Html.divc "panel-block" core

            Html.divc "panel-block" [

                Html.divc "left-left" [
                    bButton "Save" [
                        text "Save"
                        onClick (fun _ -> EditorMsgType.Save SaveMsg.Requested |> lDispatch) []
                    ]
                    rButton "Cancel" [ text "Cancel"; onClick (fun _ -> Cancel |> pDispatch) [] ]
                ]
            ]
            Html.pre [ text (Core.pretty value) ]
        ]

let justModel<'tMsg> m : Model * Cmd<'tMsg> = m, Cmd.none

let init item =
    let initialTab = stateStore.TryGetValue() |> Option.map (fun cs -> cs.Tab) // |> Option.bind (fun cs -> cs.Tab |> EditorTabs.ReParse)

    justModel {
        Tab = initialTab |> Option.defaultValue IconTab
        Item = item
        Errors = List.empty
    }

module SideEffects =

    let saveStateCache (next: Model) =
        Some { Tab = next.Tab } |> stateStore.TrySetValue

let update appMode dispatchParent msg (model: Model) : Model * Cmd<EditorMsgType> =
    printfn "NavEditor update: %A" msg

    let block msg : Model * Cmd<EditorMsgType> =
        model |> MLens.addError msg |> justModel<EditorMsgType>

    match msg with
    // blocks

    | EditAcl(AclEditor.AclParentMsg.Create(aclRef, acl)) when
        model.Item.AclRefs |> Seq.exists (fun e -> e.Name = acl.Name)
        ->
        block $"Acl Create for existing acl '{acl.Name}'"

    // actions

    | TabChange t ->
        let next = { model with Tab = t }

        justModel
        <| match SideEffects.saveStateCache next with
           | Ok() -> next
           | Error e -> MLens.addError e model
    | IconMsg(IconEditor.IconEditorMsg.NameChange(name, value))

    | EditProp(name, value) ->
        let nextItem = cloneSet model.Item name value
        justModel { model with Item = nextItem }

    | EditAcl(AclEditor.AclParentMsg.TypeChange v) ->
        ParentMsg.AclTypeChange v |> dispatchParent
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

    | EditAcl(AclEditor.AclParentMsg.Create(aclRef, acl)) ->
        printfn "cloning item to add acl"
        // should we try to block repeated acl names?
        let nextItem =
            let n = nameof model.Item.AclRefs
            cloneSet model.Item n (model.Item.AclRefs |> Seq.append [ aclRef ] |> Array.ofSeq)

        justModel { model with Item = nextItem }
    | EditAcl(AclEditor.AclParentMsg.SearchRequest v) ->
        ParentMsg.AclSearchRequest v |> dispatchParent
        justModel model
    | Save(Responded(Error e)) -> MLens.addError e model |> justModel
    | Save(Responded(Ok item)) ->
        ParentMsg.Saved item |> dispatchParent
        justModel model

    // this is save requested, not resolved
    | Save SaveMsg.Requested ->
        match appMode with
        | Auth token -> model, Commands.saveItem token model.Item
        | Demo -> MLens.addError "Save Not implemented in demo" model |> justModel

()

type NavEditorProps = {
    AppMode: ConfigType<string>
    ResolvedAclParams: System.IObservable<Map<string, AclDisplay>>
    AclTypes: Acl seq
    NavItem: NavItem
    NavItemIconObservable: System.IObservable<NavItem option>
    AclSearchResponse: AclSearchResponse option
    DispatchParent: Dispatch<ParentMsg>
}

// renames will go a different route, no path editing
let renderEditor props =

    toGlobalWindow "navEditor_props" props

    let store, dispatch =
        props.NavItem
        |> Store.makeElmish init (update props.AppMode props.DispatchParent) ignore

    toGlobalWindow "navEditor_model" store.Value

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
                                            props.NavItemIconObservable
                                            |> Observable.choose id
                                            |> Observable.map (fun v -> v.Icon)
                                        PropValue = value.Icon
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
                                    AclTypes = props.AclTypes
                                    ResolvedParams = props.ResolvedAclParams
                                    AclSearchResponse = props.AclSearchResponse
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
                    props.DispatchParent
        )
    ]
