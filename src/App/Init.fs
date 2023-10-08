module App.Init

open System
open Fable.Core.JsInterop
open Core

let console = Fable.Core.JS.console

// might be able to do it without the parent lib:
// see also: https://fontawesome.com/v5/docs/apis/javascript/icon-library

[<Literal>]
let iconPackagePath = "@fortawesome/free-brands-svg-icons"


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
Fable.Core.JsInterop.importAll "@fortawesome/fontawesome-svg-core/styles.css"

let library : FALib = import "library" "@fortawesome/fontawesome-svg-core"

type IconDefinition =
    abstract prefix: string
    abstract iconName: string
    abstract icon: obj[]
    abstract member html: string[]
type IconDescriptor = { prefix: string; iconName: string}
let icon' (_:obj) : IconDefinition option = import "icon" "@fortawesome/fontawesome-svg-core"
library.add(fab)

let allIcons = lazy(
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
allIcons.Value|> printfn "%A"
// printfn "Done"

let icon (iconDescriptor: IconDescriptor) =
    if not (allIcons.Value |> Set.contains iconDescriptor.iconName) then
        printfn "Warning: %s was not found in icon list" iconDescriptor.iconName

    icon' iconDescriptor
// dom.i2svg()
dom.watch()