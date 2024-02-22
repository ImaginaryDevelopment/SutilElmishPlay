module App.Components.Admin.NavUI

open BReusable

open Sutil
open Sutil.Core
open Sutil.CoreElements

open App.Adapters.Schema
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api.Schema
open App.Adapters.Api.Shared

open App.Components.Gen
open App.Adapters.Api.Mapped
open Core

type Model = {
    // NavItem: NavItem

    Item: NavItem
    Acls: (AclData * AclType) list
}

module Style =
    open Sutil.Styling
    open type Feliz.length
    open type Feliz.borderStyle

    // https://github.com/BulmaTemplates/bulma-templates/blob/master/css/admin.css
    let css =
        // navbar is ~30
        let zOverlay = 31

        [
            rule ".flyover" [
                Css.animationDuration (System.TimeSpan.FromMilliseconds 700)
                Css.animationTimingFunctionEaseIn
                Css.zIndex <| zOverlay + 5
            ]
        // rule ".full-overlay" [
        ]

    let withCss = withStyle css

type Msg =
    | ItemTypeChange of NavItemType
    | AclSearchRequest of AclRefValueArgs
    | AclSearchResolve of Result<AclSearchResult, exn>
    | AclParamResolveRequest of AclRefLookup

module Commands =
    let runAclSearch token aclRefValueArgs =
        let f x =
            async {
                let! resp = searchAclRefValues token x

                match resp with
                | Ok v -> return Msg.AclSearchResolve(Ok v)
                | Error e -> return Msg.AclSearchResolve(Error(e))
            }

        Cmd.OfAsync.either f aclRefValueArgs id (fun ex -> Msg.AclSearchResolve(Error(ex)))

// does this need to fire off resolves for ACLs?
let init token item () =

    {
        Item = item
        Acls = List.empty
    // Focus = FocusType.Creator NameFocus
    },
    Cmd.none

let private update token msg (model: Model) : Model * Cmd<Msg> =
    match msg with

    | ItemTypeChange nit ->
        if model.Item.Type <> nit then
            {
                model with
                    Item = { model.Item with Type = nit }
            },
            Cmd.none
        else
            eprintfn "Unexpected type change attempt : %A to %A" model.Item.Type nit
            model, Cmd.none

    | AclSearchRequest aclRefValueArgs ->
        let cmd = Commands.runAclSearch token aclRefValueArgs

        model, cmd

module RenderHelpers =

    let renderErrors title errors = [
        Attr.title title
        Attr.classes [ "help"; "is-danger" ]
        text (String.concat "," errors)
    ]

    let renderErrorMapMaybe (vErrors: Map<'t option, _> option) fToString (x: 't option) =
        vErrors
        |> Option.bind (fun eMap -> eMap |> Map.tryFind x)
        |> Option.defaultValue List.empty
        |> function
            | [] -> []
            | errors -> renderErrors (fToString x) errors

    let getError vErrors =
        renderErrorMapMaybe vErrors (function
            | None -> ""
            | Some v -> v)

// None,None => Create new root folder
// Some _, None -> Create new child - Link
// None, Some => Edit folder
// Some _, Some _ -> Edit child - Link
type NavUIFocus =
    | Parent of NavItem
    | Child of parent: NavItem * child: NavItem

type NavUIProps = { Focus: NavUIFocus }

let (|CreateRootFolder|CreateChild|EditFolder|EditChild|) = // InvalidAttempt|) =
    function
    | { Focus = Parent ni } when ni.Id = NavId "" -> CreateRootFolder ni
    | { Focus = Parent ni } -> EditFolder ni
    | { Focus = Child(pni, ni) } ->
        if ni.Id = NavId "" then
            CreateChild(pni, ni)
        else
            EditChild(pni, ni)

let view token (props: NavUIProps) =
    toGlobalWindow "NavUI_props" props
    printfn "NavUI render"

    let item, isCreate, hasParent =
        match props with
        | CreateRootFolder ni -> ni, true, false
        | EditFolder ni -> ni, false, false
        | CreateChild(_, ni) -> ni, true, true
        | EditChild(_, ni) -> ni, false, true


    let store, dispatch = () |> Store.makeElmish (init token item) (update token) ignore
    // if parent is empty the only type can be folder

    toGlobalWindow "NavUI_model" store.Value

    let vStore =
        store
        |> Store.mapRStore
            {
                UseEquality = true
                DebugTitle = None
            }
            (fun model ->
                let vResult = ValidNavItem.ValidateNavItem model.Item
                vResult |> Option.ofResult)

    let renderErrors nameOpt =
        match nameOpt with
        | ValueString name -> [

            Bind.el (
                vStore |> Store.map (fun (_, vErrors) -> vErrors),
                fun v -> fragment <| RenderHelpers.getError v (Some name)
            )
          ]
        | _ ->
            let elems = RenderHelpers.getError (snd vStore.Value) None
            elems

    Html.divc "container flyover" [
        Html.h1 [
            Attr.className "title"
            Bind.el (
                store |> Store.map (fun model -> model.Item.Type),
                fun itemType ->
                    if itemType = NavItemType.Folder then
                        "FolderOpen"
                    else
                        "Link"
                    |> App.Init.IconSearchType.MuiIcon
                    |> Icons.tryIcon
            )
            text store.Value.Item.Name
        ]
        formField [
            if isCreate then text "Create Type" else text "Type"
        ] [

            Html.divc "select" [
                Html.select [
                    let fChange =
                        Handlers.onValueChangeIf dispatch (fun v ->
                            NavItemType.TryParse v |> Option.map Msg.ItemTypeChange)

                    text "ItemType"
                    Attr.className "select"
                    // TODO: if this should not be editable disable it
                    // Attr.disabled disabled
                    Attr.disabled true

                    // match props with
                    // | CreateChild item -> fChange
                    // | CreateRootFolder _ ->
                    // | EditFolder _ -> Attr.disabled true
                    // | EditChild _ -> Attr.disabled true
                    // | InvalidAttempt _ -> ()


                    // Html.option [ text "" ]
                    for o in NavItemType.All do
                        Html.option [
                            Attr.value (string o)
                            text (string o)
                            if store.Value.Item.Type = o then
                                Attr.selected true
                        ]
                ]
            ]
        ]
        <| renderErrors "Type"
        formField [ text "Name" ] [
            textInput
                {
                    Titling = "NavUI.Name"
                    Value = store |> Store.map (fun v -> v.Item.Name)
                    OnChange =
                        (fun value ->
                            store.Update(fun old -> {
                                old with
                                    Item = { old.Item with Name = value }
                            }))
                    DebounceOverride = None
                }
                []

        ]
        <| renderErrors "Name"

    ]
    |> Style.withCss