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

let allMuiIcons = lazy(
    // App.Adapters.Mui.all
    keys mui
    |> Seq.truncate 4
    |> Seq.map(fun k ->
        console.log(k)
        let v : obj = getValue mui k
        let g : obj = getValue v "type"
        let elem : obj = getValue g "render" ()
        let props : obj = getValue elem "props"
        let children : obj = getValue props "children"
        let props2 : obj = getValue children "props"
        let d : string = getValue props2 "d"
        console.log(d)
        // console.log(k,Core.pretty v)
        k,d
    )
    |> Map.ofSeq
)

// printfn "Mui: %A" allMuiIcons.Value
let getMuiIcon (name:string) =
    allMuiIcons.Value |> Map.tryFind name

    // "@fortawesome/fontawesome-svg-core": "^6.4.2",
    // "@fortawesome/free-brands-svg-icons": "^6.4.2",
// importAll "@fortawesome/fontawesome-svg-core"
// importAll "@fortawesome/free-brands-svg-icons"
// importAll "@fortawesome/fontawesome-free/js/all.min.js"
// import { library } from '@fortawesome/fontawesome-svg-core'
// importAll "@fortawesome/fontawesome-svg-core\\styles.css"
// let fab:obj = import "fab" "@fortawesome/free-brands-svg-icons"

let private fab: obj = import "fab" iconPackagePath
type FALib =
    abstract member add: pack:obj * [<ParamArray>] items : obj[] -> unit

type FADom =
    abstract member i2svg: unit -> unit
    // https://fontawesome.com/v5/docs/apis/javascript/methods#dom-i2svg-params
    abstract member watch: unit -> unit

// https://stackoverflow.com/questions/52376720/how-to-make-font-awesome-5-work-with-webpack
let dom:FADom = import "dom" "@fortawesome/fontawesome-svg-core"
// // node_modules/@fortawesome/fontawesome-free/css/all.css
// Fable.Core.JsInterop.importAll "@fortawesome/fontawesome-free/css/all.css"

// needs css-loader:
// Fable.Core.JsInterop.importAll "@fortawesome/fontawesome-svg-core/styles.css"

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
// printfn "Printing icon list"
// allFAIcons.Value|> printfn "%A"
// printfn "Done"

let icon =
    function
    | FAIcon iconName ->
        if not (allFAIcons.Value |> Set.contains iconName) then
            printfn "Warning: %s was not found in fa icon list" iconName
        icon' {prefix= "fab"; iconName= iconName}
        |> Option.map FaResult
    | MuiIcon name ->
        match allMuiIcons.Value |> Map.tryFind name with
        | None ->
            eprintfn "Warning: %s was not found in mui icon list" name
            None
        | Some d ->
            Some (MuiResult d)

// dom.i2svg()
dom.watch()