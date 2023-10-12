module App.Components.AclEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
open App.Adapters
open App.Adapters.Api
open App.Adapters.Html
open App.Adapters.Bulma

module Handlers = App.Adapters.Html.Handlers

type AclType =
    | Selectable of values: string[] option * multiValue: bool
    | Reference of multiValue: bool

type AclEditorMsg = 
    // None is a valid option
    | AclTypeChange of AclType option

// allow them to edit or create one
let renderEditor (acl: AclRef option) (aclTypes: AclType seq) =
    Html.divc "panel" [
        Html.pc "panel-heading" [
            text "Acl!"
        ]
        Html.divc "panel-block" [
            text "hello world"
        ]
    ]