module App.Components.NavCreator
// perhaps we host the acl editor inside this, so the outer controls can work?
// but then how do we restrict edit api commands?

open BReusable

open Sutil
open Sutil.CoreElements

open App.Adapters.Api
open App.Adapters.Html
open App.Adapters.Config
open App.Adapters.Bulma

open Core



type FocusType =
    | Creator
    | Editor

type Model = {
    // NavItem: NavItem
    ItemType: NavItemType
    Path: string
    Name: string
    Icon: string
    Acls: (AclRef * Acl) list
    Focus: FocusType
}

module MLens =
    let getIcon x = x.Icon

type ParentMsg =
    | CreateNavItem of CreatingNavItem
    | EditorParentMsg of NavEditor.ParentMsg

type Msg =
    | ItemTypeChange of NavItemType
    | NameChange of string
    | PathChange of string
    | EditorMsg of NavEditor.ChildParentMsg

let inline justModel m = m, Cmd.none

let init path : Model * Cmd<Msg> =
    {
        ItemType = NavItemType.Link
        Path = path
        Name = ""
        Icon = "City"
        Acls = List.empty
        Focus = FocusType.Creator
    },
    Cmd.none

let update (dispatchParent: Dispatch<ParentMsg>) (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    printfn "NavCreator Update running: %A ('%s')" msg model.Name

    match msg with
    | ItemTypeChange nit -> { model with ItemType = nit }, Cmd.none
    | PathChange next -> { model with Path = next }, Cmd.none
    | NameChange next -> { model with Name = next }, Cmd.none
    | EditorMsg NavEditor.ChildParentMsg.GotFocus -> { model with Focus = FocusType.Editor }, Cmd.none
    | EditorMsg(NavEditor.ChildParentMsg.ParentMsg pm) ->
        dispatchParent (ParentMsg.EditorParentMsg pm)
        model, Cmd.none


let private renderCreationEditor (store: IStore<Model>) dispatch dispatchParent = [
    Html.select [
        text "ItemType"
        Attr.className "select"
        // Attr.disabled disabled
        Handlers.onValueChangeIf dispatch (fun v -> NavItemType.TryParse v |> Option.map Msg.ItemTypeChange)
        // Html.option [ text "" ]
        for o in NavItemType.All do
            Html.option [
                Attr.value (string o)
                text (string o)
                if store.Value.ItemType = o then
                    Attr.selected true
            ]
    ]
    formField [ text "Name" ] [
        Html.inputc "input" [
            type' "text"
            // autofocus
            Attr.value store.Value.Name
            Handlers.onValueInputD 300 dispatch Msg.NameChange
        ]
    ]
    formField [ text "Create" ] [
        Bind.el (
            store |> Store.map (fun s -> s.Name, s.ItemType, s.Acls),
            fun (name, itemType, acls) ->
                bButton "Create" [
                    text "Create"
                    let niCt =
                        match itemType with
                        | NavItemType.Link -> NavItemCreateType.Link store.Value.Path
                        | NavItemType.Folder -> NavItemCreateType.Folder

                    let cni: CreatingNavItem = {
                        AclRefs = acls |> Seq.map fst |> Array.ofSeq
                        Path = store.Value.Path
                        // required
                        Type = niCt
                        // required
                        Name = name
                        Description = null
                        Icon = store.Value.Icon
                        Weight = 0
                        Enabled = false
                        Url = null
                        HasUrlKey = false
                    }

                    // should we check for dupes here and disable if so?
                    if String.isValueString name |> not then
                        Attr.disabled true
                    else
                        onClick (fun _ -> ParentMsg.CreateNavItem cni |> dispatchParent) []
                ]
        )
    ]
]

type AclCreatorProps = {
    DispatchParent: Dispatch<ParentMsg>
    AppMode: ConfigType<string>
    ResolvedAcls: System.IObservable<Map<string, AclDisplay>>
    Path: string
    AclTypes: Acl seq
    AclSearchResponse: AclSearchResponse option
}

let renderAclCreator (props: AclCreatorProps) =
    toGlobalWindow "navCreator_props" props

    let store, dispatch =
        props.Path |> Store.makeElmish init (update props.DispatchParent) ignore

    Html.div [

        disposeOnUnmount [ store ]

        Html.divc "box" [ yield! renderCreationEditor store dispatch props.DispatchParent ]
        Html.divc "box" [
            Bind.el (
                store,
                fun model ->
                    // Bind.el2 props.Path
                    let item = {
                        Id = ""
                        Path = model.Path
                        Parent = "Parent"
                        Type = model.ItemType
                        Name = model.Name
                        Description = "Description"
                        Icon = model.Icon
                        Weight = 0
                        Enabled = true
                        Url = "url"
                        HasUrlKey = false
                        AclRefs = Array.empty
                    }

                    NavEditor.renderEditor {
                        Core = {
                            AppMode = props.AppMode
                            AclTypes = props.AclTypes
                            NavItem = item
                            IsFocus = model.Focus = FocusType.Editor
                            AclSearchResponse = props.AclSearchResponse
                            EditorMode = NavEditor.EditorMode.Child("NavCreator", Msg.EditorMsg >> dispatch)
                        }
                        ResolvedAclParams = props.ResolvedAcls
                        NavItemIconObservable = store |> Store.map MLens.getIcon
                    }
            )

        ]
    ]
