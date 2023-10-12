module BReusable


let (|ValueString|NonValueString|) value =
    if System.String.IsNullOrWhiteSpace value then
        NonValueString ()
    else
        ValueString value


module Option =
    let ofValueString =
        function
        | ValueString v -> Some v
        | _ -> None

[<RequireQualifiedAccess>]
module String =
    let toLower =
        function
        | ValueString v -> v.ToLowerInvariant()
        | v -> v
        
module Result =
    let ofChoice =
        function
        | Choice1Of2 x -> Ok x
        | Choice2Of2 e -> Error e
module Async =
    let map f x =
        async{
            let! value = x
            return f value
        }
    let catch x =
        x
        |> Async.Catch
        |> map Result.ofChoice
    let ofResult x =
        async {
            return x
        }
