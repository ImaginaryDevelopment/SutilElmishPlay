module BReusable

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
