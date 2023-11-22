[<RequireQualifiedAccess>]
module App.Components.NavShared

open BReusable
open Core

open Sutil
open Sutil.CoreElements

open App.Adapters.Api
open App.Adapters.Html

open App.Components.Gen.Icons

let renderEditorFrame (name, path, value: 't) core siblings =
    Html.divc "panel" [
        data_ "file" "NavShared"
        data_ "method" "renderEditorFrame"
        // path
        Html.pc "panel-heading" [
            if value.Type = Folder then
                tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
            Html.span [ text $"{value.Name}: {value.Path}" ]
        ]

        Html.divc "panel-block" core

        yield! siblings
        Html.pre [ text (Core.pretty value) ]
    ]
