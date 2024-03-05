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

// let css = [
//     // rule "span.info" Gen.CssRules.titleIndicator
//     rule ".panel.editorFrame>.panel-block>.box" [
//         Css.minHeight (px 400)
//         Css.marginBottom 0
//         Css.marginTop 0
//         Css.marginLeft (px 5)
//         Css.marginRight (px 5)
//     ]
//     rule "panel-heading" [ Css.marginLeft (px 10) ]
// ]

[<System.Obsolete>]
type ParentMsg =
    | AclTypeChange of AclType
    | AclSearchRequest of AclRefValueArgs
    | AclParamResolveRequest of AclRefLookup

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
