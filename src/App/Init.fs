module App.Init

open System
open Fable.Core.JsInterop

importAll "@fortawesome/fontawesome-svg-core"
importAll "@fortawesome/free-brands-svg-icons"
// import { library } from '@fortawesome/fontawesome-svg-core'
// importAll "@fortawesome/fontawesome-svg-core\\styles.css"
let fab:obj = import "fab" "@fortawesome/free-brands-svg-icons"

type FALib =
    abstract member add: pack:obj * [<ParamArray>] items : obj[] -> unit

let library : FALib = import "library" "@fortawesome/fontawesome-svg-core"

library.add(fab)