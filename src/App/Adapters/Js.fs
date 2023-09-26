module Core

open Fable.Core

[<Emit("JSON.stringify($0,null,\" \")")>]
let pretty<'t> (x:'t) =
    JS.JSON.stringify(x, Unchecked.defaultof<_>," ") //JSON.stringify(data, null, "  ") 

()