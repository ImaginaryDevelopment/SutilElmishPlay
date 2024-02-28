[<RequireQualifiedAccess>]
module App.Components.NavShared

open BReusable
open Core

open Sutil
open Sutil.CoreElements

open App.Adapters.Icons

open App.Adapters.Api.Schema
open App.Adapters.Api.Shared
open App.Adapters.Html

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

[<System.Obsolete>]
type ParentMsg =
    | AclTypeChange of AclType
    | AclSearchRequest of AclRefValueArgs
    | AclParamResolveRequest of AclRefLookup

let renderEditorFrame (value: NavItem) core siblings =
    Html.divc "panel editorFrame" [
        data_ "file" "NavShared"
        data_ "method" "renderEditorFrame"
        // path
        Html.pc "panel-heading" [
            if value.Type = Folder then "FolderOpen" else "Link"
            |> IconSearchType.MuiIcon
            |> tryIcon
            Html.span [ text $"%s{value.Name}: %s{value.Path}" ]
        ]

        Html.divc "panel-block" core

        yield! siblings
        Html.pre [ text (Core.pretty value) ]
    ]
    |> withStyle css

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
