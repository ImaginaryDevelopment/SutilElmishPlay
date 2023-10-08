module Core

open Fable.Core

// https://medium.com/@zaid.naom/f-interop-with-javascript-in-fable-the-complete-guide-ccc5b896a59f
[<Emit("JSON.stringify($0,null,\" \")")>]
let pretty<'t> (x:'t) =
    JS.JSON.stringify(x, Unchecked.defaultof<_>," ") //JSON.stringify(data, null, "  ")

[<Emit("Object.keys($0)")>]
let keys x : string[] = jsNative

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
()