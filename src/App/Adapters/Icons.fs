module App.Adapters.Icons

open System
open Fable.Core.JsInterop
open Sutil
open Core

type IconSearchType =
    | FAIcon of string
    | MuiIcon of string


let mutable private initIconDiag = false

// might be able to do it without the parent lib:
// see also: https://fontawesome.com/v5/docs/apis/javascript/icon-library

[<Literal>]
let iconPackagePath = "@fortawesome/free-brands-svg-icons"

[<Literal>]
let muiIconPackagePath = "@mui/icons-material"

module Mui =
    let private mui: obj = importAll muiIconPackagePath

    let allMuiIcons = App.Adapters.Mui.all

    toGlobal "getMuiIcons" (fun () -> allMuiIcons.Keys |> Array.ofSeq)

    // printfn "Mui: %A" allMuiIcons.Value
    let getMuiIcon (name: string) = allMuiIcons |> Map.tryFind name

type FAIconDefinition =
    abstract prefix: string
    abstract iconName: string
    abstract icon: obj[]
    abstract member html: string[]

type FAIconDescriptor = { prefix: string; iconName: string }

module FA =
    let private fab: obj = import "fab" iconPackagePath

    type FALib =
        abstract member add: pack: obj * [<ParamArray>] items: obj[] -> unit


    let library: FALib = import "library" "@fortawesome/fontawesome-svg-core"

    let allFAIcons =
        lazy
            (keys fab
             |> Seq.map (fun k ->
                 // log(k)
                 let v: FAIconDescriptor = getValue fab k
                 // log(k,Core.pretty v)
                 v.iconName)
             |> Set.ofSeq)

    toGlobal "getFaIcons" (fun () -> Set.toArray allFAIcons.Value)

    let icon' (_: obj) : FAIconDefinition option =
        import "icon" "@fortawesome/fontawesome-svg-core"

    library.add fab

type IconResultType =
    | FaResult of FAIconDefinition
    | MuiResult of string

let tryFindIcon (value: string) =
    if Mui.allMuiIcons |> Map.containsKey value then
        Some(MuiIcon value)
    elif FA.allFAIcons.Value |> Set.contains value then
        Some(FAIcon value)
    else
        if initIconDiag then
            eprintfn "Could not find icon '%s'" value

        None


module Styling =
    open Sutil.Styling
    open type Feliz.length

    let mutable private initIconDiag = false

    let css = [
        // rule based on nav menu
        rule "svg.mui" [
            // Css.maxWidth (px 24)
            // Css.maxHeight (px 24)
            Css.height (em 1.0)
            Css.width (em 1.0)
            Css.flexShrink 0
            Css.fontSize (rem 1.5)
            Css.color ("rgb(61, 60, 65)")
        // Css.padding (px 15.0)
        ]
    ]

let icon =
    function
    | FAIcon iconName ->
        if initIconDiag && not (FA.allFAIcons.Value |> Set.contains iconName) then
            eprintfn "Warning: %s was not found in fa icon list" iconName

        FA.icon' { prefix = "fab"; iconName = iconName } |> Option.map FaResult
    | MuiIcon name ->
        match Mui.allMuiIcons |> Map.tryFind name with
        | None ->
            if initIconDiag then
                eprintfn "Warning: %s was not found in mui icon list" name

            None
        | Some d -> Some(MuiResult d)

let tryIcon x =
    // bulma icon class
    Html.spanc "icon" [
        match icon x, x with
        | None, IconSearchType.FAIcon x
        | None, IconSearchType.MuiIcon x ->
            // text $"missing:{x}"
            App.Adapters.Html.data_ "icon" x
            Attr.title x
            Bulma.FontAwesome.fa x
        | Some(FaResult v), _ ->
            if v.html.Length <> 1 then
                eprintfn "Unexpected fa html len: %i" v.html.Length

            let html = v.html[0]
            App.Adapters.Html.data_ "icon" v.iconName
            Html.parse html
        | Some(MuiResult dPath), _ ->
            App.Adapters.Html.data_ "icon" "(dPath)"

            Svg.svg [ Attr.className "mui"; Svg.path [ Attr.d dPath ] ]
            |> Styling.withStyle Styling.css
    ]

()
