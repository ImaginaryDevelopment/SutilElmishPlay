module App.Init
// purpose: Hold the initialization code for ? (primarily icons it seems)

open System
open Fable.Core.JsInterop
open Core

let mutable initIconDiag = false
// might be able to do it without the parent lib:
// see also: https://fontawesome.com/v5/docs/apis/javascript/icon-library

[<Literal>]
let iconPackagePath = "@fortawesome/free-brands-svg-icons"

[<Literal>]
let muiIconPackagePath = "@mui/icons-material"

type IconSearchType =
    | FAIcon of string
    | MuiIcon of string

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

    type FADom =
        abstract member i2svg: unit -> unit
        // https://fontawesome.com/v5/docs/apis/javascript/methods#dom-i2svg-params
        abstract member watch: unit -> unit

    // https://stackoverflow.com/questions/52376720/how-to-make-font-awesome-5-work-with-webpack
    let dom: FADom = import "dom" "@fortawesome/fontawesome-svg-core"

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

// dom.i2svg()
FA.dom.watch ()
