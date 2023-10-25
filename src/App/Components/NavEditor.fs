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

// [<Fable.Core.Erase>]
type EditorTabs =
    | IconTab
    | AclTab

    static member ReParse(x: EditorTabs) =
        match string x with
        | y when y = string IconTab -> Some IconTab
        | y when y = string AclTab -> Some AclTab
        | _ -> None


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
    | AclTypeChange of string
    | AclSearchRequest of AclRefValueArgs

type EditorMsgType =
    | EditProp of prop: string * nextValue: string
    | EditAcl of AclEditor.AclParentMsg
    | TabChange of EditorTabs
    | IconMsg of IconEditor.IconEditorMsg
    | Save
    | SaveError of NavEditorErrorType

module Renderers =
    let renderIconEditor (propName, propObs) (value: string) (dispatch: Dispatch<EditorMsgType>) =
        let dispatch2 =
            function
            | App.Components.IconEditor.IconEditorMsg.NameChange(propName, value) ->
                dispatch (EditProp(propName, value))

        App.Components.IconEditor.renderIconEditor (propName, propObs) value dispatch2

    let renderEditorFrame (value: NavItem) core (lDispatch: Dispatch<EditorMsgType>) (pDispatch: Dispatch<ParentMsg>) =
        Html.divc "panel" [
            // path
            Html.pc "panel-heading" [
                if value.Type = "Folder" then
                    tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
                Html.span [ text $"{value.Name}: {value.Path}" ]
            ]

            Html.divc "panel-block" core

            Html.divc "panel-block" [
                Html.divc "left-left" [
                    Html.buttonc "button" [ text "Save"; onClick (fun _ -> ParentMsg.Saved value |> pDispatch) [] ]
                    Html.buttonc "button" [ text "Cancel"; onClick (fun _ -> Cancel |> pDispatch) [] ]
                ]
            ]
            Html.pre [ text (Core.pretty value) ]
        ]

let init item =
    let initialTab =
        stateStore.TryGetValue() |> Option.bind (fun cs -> cs.Tab |> EditorTabs.ReParse)

    {
        Tab = initialTab |> Option.defaultValue IconTab
        Item = item
        Errors = List.empty
    }

module SideEffects =

    let saveStateCache (next: Model) =
        Some { Tab = next.Tab } |> stateStore.TrySetValue

let update dispatchParent msg (model: Model) =
    printfn "NavEditor update: %A" msg

    match msg with
    | TabChange t ->
        let next = { model with Tab = t }

        match SideEffects.saveStateCache next with
        | Ok() -> next
        | Error e -> MLens.addError e model
    | IconMsg(IconEditor.IconEditorMsg.NameChange(name, value))

    | EditProp(name, value) ->
        let nextItem = cloneSet model.Item name value
        { model with Item = nextItem }

    | EditAcl(AclEditor.AclParentMsg.AclTypeChange v) ->
        ParentMsg.AclTypeChange v |> dispatchParent
        model
    | EditAcl(AclEditor.AclParentMsg.AclSearchRequest v) ->
        ParentMsg.AclSearchRequest v |> dispatchParent
        model
    | SaveError e -> MLens.addError e model

    // TODO: Not implemented
    // this is save requested, not resolved
    | Save -> model

()

type NavEditorProps = {
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
        |> Store.makeElmishSimple init (update props.DispatchParent) ignore

    toGlobalWindow "navEditor_model" store.Value

    let core =
        let obsTab = store |> Store.map MLens.getTab
        let obsItem = store |> Store.map MLens.getItem

        Bind.el2 obsTab obsItem (fun (tab, value) ->
            tabs
                [
                    {
                        Name = "Icon"
                        TabClickMsg = TabChange IconTab
                        IsActive = tab = EditorTabs.IconTab
                        Render =
                            fun () ->
                                IconEditor.renderIconEditor
                                    ("Icon",
                                     props.NavItemIconObservable
                                     |> Observable.choose id
                                     |> Observable.map (fun v -> v.Icon))
                                    value.Icon
                                    (IconMsg >> dispatch) // viewNavRoot (store,dispatch)
                    }
                    {
                        Name = "Acls"
                        TabClickMsg = TabChange AclTab
                        IsActive = tab = AclTab
                        Render =
                            fun () ->
                                AclEditor.renderAclsEditor {
                                    ItemAcls = value.Acls
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
