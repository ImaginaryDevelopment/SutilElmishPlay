module App.Init

open System
open Fable.Core.JsInterop
open Core

let console = Fable.Core.JS.console

// might be able to do it without the parent lib:
// see also: https://fontawesome.com/v5/docs/apis/javascript/icon-library

[<Literal>]
let iconPackagePath = "@fortawesome/free-brands-svg-icons"
[<Literal>]
let muiIconPackagePath = "@mui/icons-material"

type IconSearchType =
    | FAIcon of string
    | MuiIcon of string

let private mui:obj = importAll muiIconPackagePath

let allMuiIcons = App.Adapters.Mui.all

// printfn "Mui: %A" allMuiIcons.Value
let getMuiIcon (name:string) =
    allMuiIcons |> Map.tryFind name

let private fab: obj = import "fab" iconPackagePath
type FALib =
    abstract member add: pack:obj * [<ParamArray>] items : obj[] -> unit

type FADom =
    abstract member i2svg: unit -> unit
    // https://fontawesome.com/v5/docs/apis/javascript/methods#dom-i2svg-params
    abstract member watch: unit -> unit

// https://stackoverflow.com/questions/52376720/how-to-make-font-awesome-5-work-with-webpack
let dom:FADom = import "dom" "@fortawesome/fontawesome-svg-core"

let library : FALib = import "library" "@fortawesome/fontawesome-svg-core"

type IconDefinition =
    abstract prefix: string
    abstract iconName: string
    abstract icon: obj[]
    abstract member html: string[]

type IconResultType =
    | FaResult of IconDefinition
    | MuiResult of string

type IconDescriptor = { prefix: string; iconName: string}

let icon' (_:obj) : IconDefinition option = import "icon" "@fortawesome/fontawesome-svg-core"
library.add(fab)

let allFAIcons = lazy(
    keys fab
    |> Seq.map(fun k ->
        // console.log(k)
        let v : IconDescriptor = getValue fab k
        // console.log(k,Core.pretty v)
        v.iconName
    )
    |> Set.ofSeq
    )

let icon =
    function
    | FAIcon iconName ->
        if not (allFAIcons.Value |> Set.contains iconName) then
            eprintfn "Warning: %s was not found in fa icon list" iconName
        icon' {prefix= "fab"; iconName= iconName}
        |> Option.map FaResult
    | MuiIcon name ->
        match allMuiIcons |> Map.tryFind name with
        | None ->
            eprintfn "Warning: %s was not found in mui icon list" name
            None
        | Some d ->
            Some (MuiResult d)

// dom.i2svg()
dom.watch()