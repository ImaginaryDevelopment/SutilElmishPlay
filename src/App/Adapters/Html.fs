module App.Adapters.Html

open BReusable

open Sutil
open Sutil.CoreElements

let data_ (name: string) value = prop.custom ($"data-{name}", value)

type ButtonType =
    | Submit
    | Reset
    | Button

let tButton title (buttonType: ButtonType) props =
    Html.buttonc "button" [ type' (string buttonType |> String.toLower); Attr.title title; yield! props ]

let bButton title props = tButton title Button props
let rButton title props = tButton title Reset props

let columns2 col1 col2 =
    Html.divc "columns" [ Html.divc "column" col1; Html.divc "column" col2 ]

let columns3 col1 col2 col3 =
    Html.divc "columns" [ Html.divc "column" col1; Html.divc "column" col2; Html.divc "column" col3 ]

module Handlers =
    let debounceDefault = 300
    // change only fires like on focus lost for text input
    let onValueChange<'t> (dispatch: 't -> unit) f =
        Sutil.CoreElements.on "change" (Core.Handlers.getValue >> f >> dispatch) List.empty

    let onValueChangeIf<'t> (dispatch: 't -> unit) f =
        Sutil.CoreElements.on "change" (Core.Handlers.getValue >> f >> Option.iter dispatch) List.empty

    let onValueChangeIfD<'t> timeoutMs (dispatch: 't -> unit) f =
        onValueChangeIf<'t> (Core.debounce dispatch timeoutMs) f

    let onValueInput<'t> (dispatch: 't -> unit) f =
        Sutil.CoreElements.on "input" (Core.Handlers.getValue >> f >> dispatch) List.empty

    let onValueInputD<'t> timeoutMs (dispatch: 't -> unit) f =
        onValueInput<'t> (Core.debounce dispatch timeoutMs) f

module Observable =
    open System
    // from https://github.com/davedawkins/Sutil/blob/57de4163fdced6cbace5f032a6c56872cdc4e80a/src/Sutil/Observable.fs#L102
    let choose (f: 'T option -> 'R option) (source: IObservable<'T option>) : IObservable<'R> =
        { new System.IObservable<_> with
            member _.Subscribe(h: IObserver<_>) =
                if isNull source then
                    failwith $"Source was null"

                let disposeA =
                    source.Subscribe(fun x ->
                        (try
                            f x
                         with ex ->
                             h.OnError ex
                             None)
                        |> Option.iter h.OnNext)

                Helpers.disposable (fun _ -> disposeA.Dispose())
        }
