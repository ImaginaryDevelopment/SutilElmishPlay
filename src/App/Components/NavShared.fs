[<RequireQualifiedAccess>]
module App.Components.NavShared

open BReusable
open Core

open Sutil
open Sutil.CoreElements

open App.Adapters.Api
open App.Adapters.Html

open App.Components.Gen.Icons

let renderEditorFrame (name, path, navItemType, value: 't) core siblings =
    Html.divc "panel" [
        data_ "file" "NavShared"
        data_ "method" "renderEditorFrame"
        // path
        Html.pc "panel-heading" [
            if navItemType = Folder then "FolderOpen" else "Link"
            |> App.Init.IconSearchType.MuiIcon
            |> tryIcon
            Html.span [ text $"%s{name}: %s{path}" ]
        ]

        Html.divc "panel-block" core

        yield! siblings
        Html.pre [ text (Core.pretty value) ]
    ]

let renderError vErrors x =
    vErrors
    |> Option.bind (fun eMap -> eMap |> Map.tryFind x)
    |> Option.defaultValue List.empty
    |> function
        | [] -> []
        | errors -> [ Attr.classes [ "help"; "is-danger" ]; text (String.concat "," errors) ]
