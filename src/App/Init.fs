module App.Init
// purpose: Hold the initialization code for ? (primarily icons it seems)

open System
open Fable.Core.JsInterop
open Core

module FA =
    type FADom =
        abstract member i2svg: unit -> unit
        // https://fontawesome.com/v5/docs/apis/javascript/methods#dom-i2svg-params
        abstract member watch: unit -> unit

    // https://stackoverflow.com/questions/52376720/how-to-make-font-awesome-5-work-with-webpack
    let dom: FADom = import "dom" "@fortawesome/fontawesome-svg-core"

// dom.i2svg()
FA.dom.watch ()
