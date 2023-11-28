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
    | EnabledChange of bool
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

    let renderPropsEditor value dispatch =
        let ff name value =
            formField [ text name ] [ textInput name value [] (fun v -> EditProp(name, v)) dispatch ]

        fun () ->
            Html.div [
                data_ "method" "renderPropsEditor"
                // ff (nameof value.Enabled) (string value.Enabled)
                formField [ text (nameof value.Enabled) ] [
                    checkbox (nameof value.Enabled) value.Enabled [] EnabledChange dispatch
                ]
                ff (nameof value.Weight) (string value.Weight)
                if value.Type = NavItemType.Link then
                    ff (nameof value.Url) value.Url
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

    | EnabledChange value ->
        justModel {
            model with
                Item = { model.Item with Enabled = value }
        }
    | IconMsg(IconEditor.IconEditorMsg.NameChange(name, value))
    | EditProp(name, value) ->
        try
            let nextItem = cloneSet model.Item name value
            justModel { model with Item = nextItem }
        with ex ->
            Core.log ex
            eprintfn $"%s{ex.Message}"

            MLens.addError $"Failed to set property: %s{name} on %s{serialize model.Item}" model
            |> justModel

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
                        Name = "Props"
                        TabType = Enabled <| TabChange EditorTabs.MainTab
                        IsActive = tab = EditorTabs.MainTab
                        Render = Renderers.renderPropsEditor value dispatch

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
        data_ "file" "NavEditor"
        Bind.el (
            store |> Store.map MLens.getItem,
            fun value ->
                match props.Core.EditorMode with
                | Standalone _ ->
                    let siblings = Renderers.renderFramed dispatch dispatchParent
                    App.Components.NavShared.renderEditorFrame (value.Name, value.Path, value) [ core ] siblings
                | Child _ -> core
        )
    ]
