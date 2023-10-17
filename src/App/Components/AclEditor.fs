module App.Components.AclEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
open App.Adapters
open App.Adapters.Api
open App.Adapters.Html
open App.Adapters.Bulma
open Core
open Gen.Icons

module Handlers = App.Adapters.Html.Handlers

// TODO: is this complete and a proper representation? move to api perhaps
type AclType =
    | Selectable of values: string[] option * multiValue: bool
    | Reference of multiValue: bool

type AclParentMsg =
    // None is a valid option
    | AclTypeChange of AclType option

type Msg =
    | TypeSelectChange of string
    | AclSelect of AclRef

type AclState = {
    IsNew: bool
    AclRef: AclRef
}
type Model = {
    FocusedAcl: AclState option
    ErrorQueue: (string * System.DateTime) list
}

let emptyAcl = {
        Name= ""
        Parameters= Array.empty
}

let init aclRefOpt : Model * Cmd<Msg> =
    {FocusedAcl = aclRefOpt; ErrorQueue = List.empty}, Cmd.none

[<RequireQualifiedAccess>]
module private MLens =
    let updateAclRef item f model = {model with FocusedAcl = Some {item with AclRef = f item.AclRef}}
    let getFocusedAcl model = model.FocusedAcl
    let getErrors model = model.ErrorQueue
    let addError text model = {model with ErrorQueue= (text, System.DateTime.Now):: getErrors model}

let update (itemAcls: AclRef seq) (msg:Msg) (model:Model) =
    let justError title = MLens.addError title model, Cmd.none
    match msg, model with
    // blocks

    | TypeSelectChange _, {FocusedAcl = None} -> justError "Start a new acl first"
    | TypeSelectChange _, {FocusedAcl = Some {IsNew = false}} -> justError "Cannot change type on new acl"
    | TypeSelectChange v, _ when itemAcls |> Seq.exists(fun acl -> acl.Name = v) -> justError "Cannot change to existing acl type"

    // | TypeSelectChange v, {FocusedAcl = Some x} -> {model with FocusedAcl= Some {x with AclRef={x.AclRef with Name=v}}}
    | TypeSelectChange v, {FocusedAcl = Some x} -> model |> MLens.updateAclRef x (fun x -> {x with Name=v}) , Cmd.none
    // TODO: should we warn if they have unsaved changed and select another acl?
    | AclSelect v, _ -> {model with FocusedAcl= Some {IsNew= false; AclRef=v}}, Cmd.none

module Renderers =
    open Gen
    let renderAclTypeSelector (aclTypes: Acl seq) selectedType disabled (dispatch:Dispatch<Msg>) = 
        log aclTypes

        Html.select [
            text "Acl!"
            Attr.className "select"
            Attr.disabled disabled
            Handlers.onValueChangeIf dispatch (fun v -> Option.ofValueString v |> Option.map Msg.TypeSelectChange)
            Html.option [
                text ""
            ]
            for o in aclTypes do
                Html.option [
                    Attr.value o
                    text o.Name
                    if selectedType = o.Name then
                        Attr.selected true
                ]
        ]

    let renderAclParams (idMap:Map<string,RemoteData<string>>) (aclType: Acl) (item: AclRef) =
        Html.div [
            Html.ul [
                for p in item.Parameters do
                    Html.li [
                        match idMap |> Map.tryFind p with
                        | Some NotRequested
                        | Some InFlight
                        | None ->
                            text p
                        | Some (Responded(Ok display)) ->
                            Attr.title p
                            text display
                        | Some (Responded(Error e)) ->
                            Attr.title (string e)
                            text p
                    ]
            ]

        ]

    let renderAcl selectedType (item:AclRef) dispatch =
        let isActiveButton = item.Name = selectedType
        Html.divc "columns" [
            Html.divc "column is-one-fifth buttonColumn" [
                Html.button [
                    Attr.title "Edit Acl"
                    Attr.classes [
                        "button"
                        "is-small"
                        if isActiveButton then
                            "is-light is-primary"
                        else
                            "is-link"
                    ]
                    tryIcon (App.Init.IconSearchType.MuiIcon "Edit")
                    Attr.disabled isActiveButton
                    if not isActiveButton then
                        onClick (fun _ -> item |> Msg.AclSelect |> dispatch) List.empty
                ]
            ]
            Html.divc "column is-four-fifths" [
                Attr.classes [
                    if isActiveButton then
                        "has-text-weight-bold"
                ]
                text item.Name
            ]
            Html.divc "column is-one-fifth" [
                text (string item.Parameters.Length)
            ]
        ]

open Sutil.Styling
open type Feliz.length

let css = [
    rule "span.info" Gen.CssRules.titleIndicator

    // not working
    // rule ".panel-block>*" [ 
    //     Css.custom("!width", "100%")
    // ]

    // rule "button.button" [
    // //     Css.marginBottom (px -15)
    // //     Css.marginTop (px -15)
    //     // Css.fontSize (em 0.7)
    //     Css.paddingTop (px 0)
    //     Css.paddingBottom (px 0)
    //     Css.marginBottom (px 0)
    //     Css.marginTop (px 0)
    // ]

    // rule "button.button>.icon" [
    //     // Css.fontSize (rem 0.1)
    //     Css.paddingTop (px 0)
    //     Css.paddingBottom (px 0)
    //     Css.marginBottom (px 0)
    //     Css.marginTop (px 0)
    // ]

    rule "div.buttonColumn" [
        Css.height (em 1.0)
        Css.width (em 2.5)
        Css.flexShrink 0
        Css.marginRight (px 5)
    ]

    rule ".columns" [
        Css.marginTop (px 0)
        Css.marginBottom (px 0)
        Css.marginLeft (px 5)
    ]
]

// allow them to edit or create one
let renderAclsEditor (itemAcls:AclRef seq) (aclTypes:Acl seq) (dispatchParent:Dispatch<AclParentMsg>) =
    let store, dispatch =
        itemAcls
        |> Seq.tryHead
        |> Option.map (fun v -> {IsNew=false;AclRef=v})
        |> Store.makeElmish init (update itemAcls) ignore

    Html.div [
        disposeOnUnmount [ store ]
        store |> Store.map MLens.getErrors |> Gen.ErrorHandling.renderErrorDisplay

        Bind.el(store |> Store.map MLens.getFocusedAcl, fun focusOpt ->
            let selectedType = focusOpt |> Option.map(fun item -> item.AclRef.Name) |> Option.defaultValue "" // String.toLower store.Value.FocusedAcl.Name
            let selectedAclType = aclTypes |> Seq.tryFind(fun v -> v.Name = selectedType)
            Html.div [
                Renderers.renderAclTypeSelector aclTypes selectedType (Option.isSome focusOpt) dispatch
                match focusOpt with
                | Some item ->
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty item)]
                    Html.spanc "info" [ text "*"; Attr.title (Core.pretty selectedAclType)]
                | None ->
                    ()
            ]
        )
        Html.h2 [text "Existing Acls"]

        Bind.el(store |> Store.map MLens.getFocusedAcl, fun focusOpt ->
            let selectedType = focusOpt |> Option.map(fun item -> item.AclRef.Name) |> Option.defaultValue "" // String.toLower store.Value.FocusedAcl.Name
            Html.div [
                for (i,acl) in itemAcls |> Seq.indexed do
                    Html.div [
                        if i % 2 = 0 then
                            Attr.className "has-background-link-light"
                        Renderers.renderAcl selectedType acl dispatch
                        ]
                    ]
        )

    ]
    |> withStyle css