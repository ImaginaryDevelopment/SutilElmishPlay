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

type Model = {
    // NavItem: NavItem
    Item: NavItem
    Acls: (AclData * AclType) list
}

module MLens =
    let getIcon (x: Model) = x.Item.Icon
    let getItem x = x.Item
    let setItemProp model f = { model with Item = f model.Item }

type ParentMsg =
    | CreateNavItem of ValidNavItem
    | EditorParentMsg of NavShared.ParentMsg

type Msg =
    | ItemTypeChange of NavItemType
    | NameChange of string
    | PathChange of string
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
    // Focus = FocusType.Creator NameFocus
    },
    Cmd.none

let update (dispatchParent: Dispatch<ParentMsg>) (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    printfn "NavCreator Update running: %A ('%s')" msg model.Item.Name

    match msg with
    | ItemTypeChange nit -> MLens.setItemProp model (fun item -> { item with Type = nit }), Cmd.none
    | PathChange next -> MLens.setItemProp model (fun item -> { item with Path = next }) |> justModel

    | NameChange next ->
        printfn "Setting name to %s" next

        MLens.setItemProp model (fun item -> { item with Name = next }) |> justModel

    | EditorMsg(NavEditor.ChildParentMsg.ItemUpdate nextItem) -> { model with Item = nextItem }, Cmd.none
    | EditorMsg(NavEditor.ChildParentMsg.ParentMsg pm) ->
        dispatchParent (ParentMsg.EditorParentMsg pm)
        model, Cmd.none

// [<RequireQualifiedAccess>]
// type ModelState =
//     | Valid of ValidNavItem
//     | Invalid of NavItem * Map<FieldName option, string list>


type AclCreatorProps = {
    DispatchParent: Dispatch<ParentMsg>
    AppMode: ConfigType<string>
    Path: string
    AclTypes: IReadOnlyStore<AclType seq>
}

type FocusableFormFieldArgs = {
    Name: string
    Value: System.IObservable<string>
    OnChange: string -> unit
}

let renderAclCreator (props: AclCreatorProps) =
    toGlobalWindow "navCreator_props" props
    printfn "Render NavCreator"

    if Seq.isEmpty props.AclTypes.Value then
        eprintfn "No AclTypes found"

    let store, dispatch =
        props.Path |> Store.makeElmish init (update props.DispatchParent) ignore

    toGlobalWindow "navCreator_model" store.Value

    let focusableFormField getError (fffArgs: FocusableFormFieldArgs) =
        formField [ text fffArgs.Name ] [

            textInput
                {
                    Titling = "NavCreator." + fffArgs.Name
                    Value = fffArgs.Value
                    OnChange = fffArgs.OnChange
                    DebounceOverride = None
                }
                []
        ]
        <| getError (Some fffArgs.Name)

    let renderCreationEditor getError allErrors (itemStore: IReadOnlyStore<NavItem>) vItem = [

        columns2 [] [
            if store.Value.Item.Type = Link then
                focusableFormField getError {
                    Name = "Path"
                    Value = itemStore |> Store.map (fun item -> item.Path)
                    OnChange = Msg.PathChange >> dispatch
                }
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
                                if itemStore.Value.Type = o then
                                    Attr.selected true
                            ]
                    ]
                ]
            ]
            <| getError (Some "Type")

        ]
        focusableFormField getError {
            Name = "Name"
            Value = store |> Store.map (fun model -> model.Item.Name)
            OnChange = Msg.NameChange >> dispatch
        }

        formField [ text "Create" ] [
            bButton "Create" [
                text "Create"

                // let cni = MLens.modelToCreatingItem store.Value

                Bind.attr ("disabled", store |> Store.map (fun x -> x.Item.Name |> String.isValueString |> not))
                // should we check for dupes here and disable if so?
                // do we need to check if vItem is out of sync with some in-memory edited but not saved copy?
                // for example, icon change not accepted
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
        // Bind.el2 (store |> Store.map MLens.getItem) (store |> Store.map MLens.getFocus) (fun (item, focus) ->
        let itemStore = store |> Store.map MLens.getItem

        Bind.el (
            itemStore,
            (fun item ->
                // this may not get recalculated when validation changes
                let eItem = store.Value.Item
                // navItem validation, validates a NavItem, validateNavItem
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

                vErrors |> Option.defaultValue Map.empty |> toGlobalWindow "navCreator_Errors"

                let getError =
                    NavShared.renderErrorMapMaybe vErrors (function
                        | None -> ""
                        | Some v -> v)

                let itemStore =
                    store
                    |> Store.mapStore
                        "NavEditorNavItem"
                        true
                        ((fun model -> model.Item), (fun nextItem (model, _) -> { model with Item = nextItem }))

                let creationItems = renderCreationEditor getError allErrors itemStore vItem

                let editor =
                    Bind.el (
                        store |> Store.map MLens.getItem,
                        fun _ ->
                            NavEditor.renderEditor {
                                AppMode = props.AppMode
                                AclTypes = props.AclTypes
                                NavItem = itemStore
                                EditorMode =
                                    NavEditor.EditorMode.Child("NavCreator", vResult, Msg.EditorMsg >> dispatch)
                            }
                    )

                App.Components.NavShared.renderEditorFrame eItem [ Html.sectionc "hero" creationItems ] [
                    Html.sectionc "section" [ editor ]
                ])
        )
    ]
