module App.Init

open System
open Fable.Core.JsInterop


    // "@fortawesome/fontawesome-svg-core": "^6.4.2",
    // "@fortawesome/free-brands-svg-icons": "^6.4.2",
importAll "@fortawesome/fontawesome-svg-core"
importAll "@fortawesome/free-brands-svg-icons"
importAll "@fortawesome/fontawesome-free/js/all.min.js"
// import { library } from '@fortawesome/fontawesome-svg-core'
// importAll "@fortawesome/fontawesome-svg-core\\styles.css"
let fab:obj = import "fab" "@fortawesome/free-brands-svg-icons"

type FALib =
    abstract member add: pack:obj * [<ParamArray>] items : obj[] -> unit

type FADom =
    abstract member i2svg: unit -> unit

// https://stackoverflow.com/questions/52376720/how-to-make-font-awesome-5-work-with-webpack
let dom:FADom = import "dom" "@fortawesome/fontawesome-svg-core"
// node_modules/@fortawesome/fontawesome-free/css/all.css
// Fable.Core.JsInterop.importAll "@fortawesome/fontawesome-free/css/all.css"
Fable.Core.JsInterop.importAll "@fortawesome/fontawesome-svg-core/styles.css"

let library : FALib = import "library" "@fortawesome/fontawesome-svg-core"

library.add(fab)
dom.i2svg()