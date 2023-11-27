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
    Link: string
    Name: string
    Icon: string
    Acls: (AclRef * Acl) list
    Enabled: bool
    Weight: int
    Focus: FocusType
}

module MLens =
    let getIcon x = x.Icon

    let modelToCreatingItem (model: Model) : CreatingNavItem =
        let niCt =
            match model.ItemType with
            | NavItemType.Link -> NavItemCreateType.Link model.Path
            | NavItemType.Folder -> NavItemCreateType.Folder

        {
            AclRefs = model.Acls |> Seq.map fst |> Array.ofSeq
            Path = model.Path
            // required
            Type = niCt
            // required
            Name = model.Name
            Description = null
            Icon = model.Icon
            Weight = 0
            Enabled = false
            Url = null
            HasUrlKey = false
        }

type ParentMsg =
    | CreateNavItem of CreatingNavItem
    | EditorParentMsg of NavEditor.ParentMsg

type Msg =
    | ItemTypeChange of NavItemType
    | NameChange of string
    | PathChange of string
    | LinkChange of string
    | EditorMsg of NavEditor.ChildParentMsg

let inline justModel m = m, Cmd.none

let init path : Model * Cmd<Msg> =
    {
        ItemType = NavItemType.Link
        Path = path
        Name = ""
        Link = ""
        Icon = "City"
        Acls = List.empty
        Weight = 0
        Enabled = false
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

let private textInput titling (value: string) children onChange dispatch =
    Html.inputc "input" [
        type' "text"
        Attr.value value
        Attr.title titling
        Attr.placeholder titling
        yield! children
        Handlers.onValueInputD Handlers.debounceDefault dispatch onChange
    ]

let private renderCreationEditor (store: IStore<Model>) dispatch dispatchParent = [
    columns3
        [
            if store.Value.ItemType = Link then
                formField [ text "Path" ] [ textInput "Path" store.Value.Path [] Msg.PathChange dispatch ]
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
                                if store.Value.ItemType = o then
                                    Attr.selected true
                            ]
                    ]
                ]
            ]

        ] [
            if store.Value.ItemType = Link then
                formField [ text "Link" ] [ textInput "Link" store.Value.Link [] Msg.LinkChange dispatch ]
        ]

    formField [ text "Name" ] [
        textInput
            "Name"
            store.Value.Name
            [
                if store.Value.Focus = FocusType.Creator then
                    autofocus
            ]
            Msg.NameChange
            dispatch
    ]
    formField [ text "Create" ] [
        Bind.el (
            store |> Store.map (fun s -> s.Name, s.ItemType, s.Acls),
            fun (name, itemType, acls) ->
                bButton "Create" [
                    text "Create"
                    let cni = MLens.modelToCreatingItem store.Value

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

let private modelToItem (model: Model) : NavItem = {
    Id = ""
    Path = model.Path
    Parent = "Parent"
    Type = model.ItemType
    Name = model.Name
    Description = "Description"
    Icon = model.Icon
    Weight = 0
    Enabled = true
    Url = model.Link
    HasUrlKey = false
    AclRefs = model.Acls |> Seq.map fst |> Array.ofSeq
}


let renderAclCreator (props: AclCreatorProps) =
    toGlobalWindow "navCreator_props" props

    let store, dispatch =
        props.Path |> Store.makeElmish init (update props.DispatchParent) ignore

    let eItem = modelToItem store.Value

    Html.div [

        disposeOnUnmount [ store ]
        App.Components.NavShared.renderEditorFrame
            (eItem.Name, eItem.Path, eItem)
            [
                Html.divc "box" [ yield! renderCreationEditor store dispatch props.DispatchParent ]
                Html.divc "box" [
                    // Bind.el2 props.Path
                    NavEditor.renderEditor {
                        Core = {
                            AppMode = props.AppMode
                            AclTypes = props.AclTypes
                            NavItem = eItem
                            IsFocus = store.Value.Focus = FocusType.Editor
                            AclSearchResponse = props.AclSearchResponse
                            EditorMode = NavEditor.EditorMode.Child("NavCreator", Msg.EditorMsg >> dispatch)
                        }
                        ResolvedAclParams = props.ResolvedAcls
                        NavItemIconObservable = store |> Store.map MLens.getIcon
                    }

                ]
            ]
            List.empty
    ]
