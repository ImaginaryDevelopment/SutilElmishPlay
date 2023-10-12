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

module Handlers = App.Adapters.Html.Handlers

// TODO: is this complete and a proper representation? move to api perhaps
type AclType =
    | Selectable of values: string[] option * multiValue: bool
    | Reference of multiValue: bool

// when a save is requested
type AclParentMsg =
    // None is a valid option
    | AclTypeChange of AclType option

type AclEditorMsg =
    | TypeSelectChange of string

type AclState = {
    IsNew: bool
    AclRef: AclRef
}
type Model = {
    FocusedAcl: AclState option
    Error: string
}

let emptyAcl = {
        Name= ""
        Parameters= Array.empty
}

let init aclRefOpt =
    {FocusedAcl = aclRefOpt; Error = null}

module MLens =
    let updateAclRef item f model = {model with FocusedAcl = Some {item with AclRef = f item.AclRef}}

let update (itemAcls: AclRef seq) msg (model:Model) =
    match msg, model with
    | TypeSelectChange _, {FocusedAcl = None} -> {model with Error = "Start a new acl first"}
    | TypeSelectChange _, {FocusedAcl = Some {IsNew = false}} -> {model with Error = "Cannot change type on new acl"}
    | TypeSelectChange v, _ when itemAcls |> Seq.exists(fun acl -> acl.Name = v) -> {model with Error = "Cannot change to existing acl type"}
    // | TypeSelectChange v, {FocusedAcl = Some x} -> {model with FocusedAcl= Some {x with AclRef={x.AclRef with Name=v}}}
    | TypeSelectChange v, {FocusedAcl = Some x} -> model |> MLens.updateAclRef x (fun x -> {x with Name=v}) 

module Renderers =
    let renderAclTypeSelector (aclTypes: Acl seq) selectedType dispatch = 
        log aclTypes

        Html.select [
            text "Acl!"
            Attr.className "select"
            Handlers.onValueChangeIf dispatch (fun v -> Option.ofValueString v |> Option.map AclEditorMsg.TypeSelectChange)
            for o in aclTypes do
                Html.option [
                    Attr.value o
                    text o.Name
                    if selectedType = o.Name then
                        Attr.selected true
                ]
        ]

let renderAcl (item:AclRef) =
    Html.div [
        text item.Name
    ]
// allow them to edit or create one
let renderAclsEditor (itemAcls: AclRef seq) (aclTypes: Acl seq) (dispatchParent:Dispatch<AclParentMsg>) =
    let store, dispatch =
        itemAcls
        |> Seq.tryHead
        |> Option.map (fun v -> {IsNew=false;AclRef=v})
        |> Store.makeElmishSimple init (update itemAcls) ignore

    let selectedType = "" // String.toLower store.Value.FocusedAcl.Name

    Html.div [
        Renderers.renderAclTypeSelector aclTypes selectedType dispatch
    ]