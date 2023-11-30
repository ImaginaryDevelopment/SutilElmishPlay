[<RequireQualifiedAccess>]
module App.Components.NavShared

open BReusable
open Core

open Sutil
open Sutil.CoreElements

open App.Adapters.Api
open App.Adapters.Html

open App.Components.Gen.Icons
open Sutil.Styling
open type Feliz.length

let css = [
    // rule "span.info" Gen.CssRules.titleIndicator
    rule ".panel.editorFrame>.panel-block>.box" [
        Css.minHeight (px 400)
        Css.marginBottom 0
        Css.marginTop 0
        Css.marginLeft (px 5)
        Css.marginRight (px 5)
    ]
]

let renderEditorFrame (value: NavItem) core siblings =
    Html.divc "panel editorFrame" [
        data_ "file" "NavShared"
        data_ "method" "renderEditorFrame"
        // path
        Html.pc "panel-heading" [
            if value.Type = Folder then "FolderOpen" else "Link"
            |> App.Init.IconSearchType.MuiIcon
            |> tryIcon
            Html.span [ text $"%s{value.Name}: %s{value.Path}" ]
        ]

        Html.divc "panel-block" core

        yield! siblings
        Html.pre [ text (Core.pretty value) ]
    ]
    |> withStyle css

let renderError vErrors x =
    vErrors
    |> Option.bind (fun eMap -> eMap |> Map.tryFind x)
    |> Option.defaultValue List.empty
    |> function
        | [] -> []
        | errors -> [ Attr.classes [ "help"; "is-danger" ]; text (String.concat "," errors) ]
