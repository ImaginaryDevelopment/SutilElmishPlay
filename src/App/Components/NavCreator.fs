module App.Components.NavCreator
// perhaps we host the acl editor inside this, so the outer controls can work?
// but then how do we restrict edit api commands?

open BReusable

open Sutil
open Sutil.CoreElements

open App.Adapters.Schema

open App.Adapters.Api
open App.Adapters.Api.Schema
open App.Adapters.Api.Mapped

open App.Adapters.Html
open App.Adapters.Config
open App.Adapters.Bulma

open Core


type FocusType =
    | Creator
    | Editor

type Model = {
    // NavItem: NavItem
    Item: NavItem
    Acls: (AclData * AclType) list
    Focus: FocusType
}

module MLens =
    let getIcon (x: Model) = x.Item.Icon
    let getItem x = x.Item
    let getFocus x = x.Focus
    let setItemProp model f = { model with Item = f model.Item }

type ParentMsg =
    | CreateNavItem of ValidNavItem
    | EditorParentMsg of NavShared.ParentMsg

type Msg =
    | ItemTypeChange of NavItemType
    | NameChange of string
    | PathChange of string
    // | LinkChange of string
    | EditorMsg of NavEditor.ChildParentMsg


let inline justModel m = m, Cmd.none

let init path : Model * Cmd<Msg> =
    {
        Item = {
            Id = NavId null
            Parent = null
            Type = NavItemType.Link
            Description = null
            Path = path
            Name = ""
            Url = ""
            HasUrlKey = false
            AclRefs = Map.empty
            Icon = "City"
            Weight = 0
            Enabled = false
        }
        Acls = List.empty
        Focus = FocusType.Creator
    },
    Cmd.none

let update (dispatchParent: Dispatch<ParentMsg>) (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    printfn "NavCreator Update running: %A ('%s')" msg model.Item.Name

    match msg with
    | ItemTypeChange nit -> MLens.setItemProp model (fun item -> { item with Type = nit }), Cmd.none
    | PathChange next -> MLens.setItemProp model (fun item -> { item with Path = next }), Cmd.none
    | NameChange next ->
        printfn "Setting name to %s" next
        MLens.setItemProp model (fun item -> { item with Name = next }), Cmd.none

    | EditorMsg NavEditor.ChildParentMsg.GotFocus -> { model with Focus = FocusType.Editor }, Cmd.none
    | EditorMsg(NavEditor.ChildParentMsg.ParentMsg pm) ->
        dispatchParent (ParentMsg.EditorParentMsg pm)
        model, Cmd.none

[<RequireQualifiedAccess>]
type ModelState =
    | Valid of ValidNavItem
    | Invalid of NavItem * Map<FieldName option, string list>


type AclCreatorProps = {
    DispatchParent: Dispatch<ParentMsg>
    AppMode: ConfigType<string>
    Path: string
    AclTypes: AclType seq
}

let renderAclCreator (props: AclCreatorProps) =
    toGlobalWindow "navCreator_props" props
    printfn "Render NavCreator"

    let store, dispatch =
        props.Path |> Store.makeElmish init (update props.DispatchParent) ignore


    let renderCreationEditor getError allErrors (item: NavItem) vItem = [

        columns2 [
            if store.Value.Item.Type = Link then
                formField [ text "Path" ] [ textInput "Path" item.Path [] Msg.PathChange dispatch ]
                <| getError (Some "Path")
        ] [
            formField [ text "Type" ] [

                Html.divc "select" [
                    Html.select [
                        text "ItemType"
                        Attr.className "select"
                        // Attr.disabled disabled
                        Handlers.onValueChangeIf dispatch (fun v ->
                            NavItemType.TryParse v |> Option.map Msg.ItemTypeChange)
                        // Html.option [ text "" ]
                        for o in NavItemType.All do
                            Html.option [
                                Attr.value (string o)
                                text (string o)
                                if item.Type = o then
                                    Attr.selected true
                            ]
                    ]
                ]
            ]
            <| getError (Some "Type")

        ]

        formField [ text "Name" ] [
            textInput
                "Name"
                store.Value.Item.Name
                [
                    if store.Value.Focus = FocusType.Creator then
                        autofocus
                ]
                Msg.NameChange
                dispatch
        ]
        <| getError (Some "Name")

        formField [ text "Create" ] [
            bButton "Create" [
                text "Create"

                // let cni = MLens.modelToCreatingItem store.Value

                // should we check for dupes here and disable if so?
                if String.isValueString item.Name |> not then
                    Attr.disabled true
                else
                    match vItem with
                    | None -> ()
                    | Some vItem -> onClick (fun _ -> ParentMsg.CreateNavItem vItem |> props.DispatchParent) []
            ]
        ]
        <| getError None
        formField [ text "Errors" ] allErrors []
    ]

    Html.div [

        disposeOnUnmount [ store ]
        Bind.el2 (store |> Store.map MLens.getItem) (store |> Store.map MLens.getFocus) (fun (item, focus) ->
            // this may not get recalculated when validation changes
            let eItem = store.Value.Item
            let vResult = ValidNavItem.ValidateNavItem eItem
            let vItem, vErrors = vResult |> Option.ofResult

            let allErrors: SutilElement list =
                vErrors
                |> Option.map (fun vm ->
                    (List.empty, vm)
                    ||> Map.fold (fun errs fnOpt fnErrs ->
                        let fn = fnOpt |> Option.defaultValue ""
                        let adds = NavShared.renderErrors fn fnErrs
                        errs @ adds))
                |> Option.defaultValue List.empty

            let getError =
                NavShared.renderErrorMapMaybe vErrors (function
                    | None -> ""
                    | Some v -> v)

            let creationItems = renderCreationEditor getError allErrors item vItem

            let editor =
                Bind.el (
                    store |> Store.map MLens.getItem,
                    fun _ ->
                        NavEditor.renderEditor {
                            Core = {
                                AppMode = props.AppMode
                                AclTypes = props.AclTypes
                                NavItem = eItem
                                IsFocus = focus = FocusType.Editor
                                EditorMode =
                                    NavEditor.EditorMode.Child("NavCreator", vResult, Msg.EditorMsg >> dispatch)
                            }
                            NavItemIconObservable = store |> Store.map MLens.getIcon
                        }
                )

            App.Components.NavShared.renderEditorFrame eItem [
                Html.sectionc "hero" creationItems
            // Html.sectionc "section" [ editor ]

            // Bind.el2 props.Path
            // ]
            ] [ editor ])
    ]
