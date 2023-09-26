module Core

open Fable.Core

[<Emit("JSON.stringify($0,null,\" \")")>]
let pretty<'t> (x:'t) =
    JS.JSON.stringify(x, Unchecked.defaultof<_>," ") //JSON.stringify(data, null, "  ") 

let mutable debug = false
let tryParse<'t> title (x:string) =
    try
        let parsed = JS.JSON.parse(x)
        if debug then printfn $"%s{title}: Parsed"
        let cast = parsed :?> 't
        if debug then printfn $"%s{title}: Cast"
        Ok cast
    with ex ->
        Error ex
()