module Core

open Fable.Core
open Fable.Core.JsInterop

let log x = Browser.Dom.console.log(x)

// https://medium.com/@zaid.naom/f-interop-with-javascript-in-fable-the-complete-guide-ccc5b896a59f
[<Emit("JSON.stringify($0,null,\" \")")>]
let pretty<'t> (x:'t) =
    JS.JSON.stringify(x, Unchecked.defaultof<_>," ") //JSON.stringify(data, null, "  ")

let serialize<'t> (x:'t) = JS.JSON.stringify(x) //JSON.stringify(data, null, "  ") 

[<Emit("Object.keys($0)")>]
let keys x : string[] = jsNative

// [<Emit("Object.assign($0)")>]
// let assign<'t> (target:obj, [<System.ParamArray>] items: obj): 't = jsNative

[<Emit("Object.assign({},$0)")>]
let clone<'t>(source:obj): 't = jsNative


[<Emit("delete $0[$1]")>]
let delete(target:obj) (name: string) = jsNative

[<Emit("debugger;")>]
let debugger () : unit = jsNative

let cloneDiagnose source =
    let v = clone source
    debugger()
    v
let cloneExcept(source:obj, exclusions: string seq) =
    let v = clone source
    exclusions |> Seq.iter(fun name -> delete v name)
    // debugger()
    v

let inline toGlobal (name:string) (value:obj) : unit =
    printfn "Adding global %s" name
    // Browser.Dom.self?(name) <- value
    Browser.Dom.window?(name) <- value

// let y = {|testing="Hello";v="world"|}
// log("Cloned:")
// log(clone y)
// log(cloneExcept(y, ["v"]))
// Fable.Core.JS.console.log("cloned:", clone y)

[<Emit("$0[$1]")>]
let getValue (x:obj) (name:string) = jsNative
let mutable debug = false
let tryParse<'t> title (x:string) : Result<'t,exn> =
    try
        let parsed = JS.JSON.parse(x)
        if debug then printfn $"%s{title}: Parsed"
        let cast = parsed :?> 't
        if debug then printfn $"%s{title}: Cast"
        Ok cast
    with ex ->
        Error ex

module Handlers =
    let getValue (e:Browser.Types.Event) : string = e.target?value
()