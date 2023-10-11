module App.Adapters.Html

open Sutil

let data_ (name:string) value = prop.custom($"data-{name}",value)

let columns2 col1 col2 =
    Html.divc "columns" [
        Html.divc "column" col1
        Html.divc "column" col2
    ]
let columns3 col1 col2 col3 =
    Html.divc "columns" [
        Html.divc "column" col1
        Html.divc "column" col2
        Html.divc "column" col3
    ]

module Handlers =
    let onValueChange<'t> (dispatch:'t -> unit) f = 
        Sutil.CoreElements.on "change" (Core.Handlers.getValue >> f >> dispatch) List.empty
    let onValueInput<'t> (dispatch: 't -> unit) f =
        Sutil.CoreElements.on "input" (Core.Handlers.getValue >> f >> dispatch) List.empty

module Observable =
    open System
    // from https://github.com/davedawkins/Sutil/blob/57de4163fdced6cbace5f032a6c56872cdc4e80a/src/Sutil/Observable.fs#L102
    let choose (f : 'T option -> 'R option) (source:IObservable<'T option>) : IObservable<'R> =
       { new System.IObservable<_> with
           member _.Subscribe( h : IObserver<_> ) =
               let disposeA = source.Subscribe( fun x ->
                   (try f x with ex -> h.OnError ex;None) |> Option.iter h.OnNext
               )
               Helpers.disposable (fun _ -> disposeA.Dispose() )
       }