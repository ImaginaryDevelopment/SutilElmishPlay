module App.Adapters.Html

open BReusable

open Sutil
open Sutil.CoreElements

let data_ (name: string) value = prop.custom ($"data-{name}", value)

let tryRender title f arg : Core.SutilElement =
    try
        f arg
    with ex ->
        eprintfn "Render failed: '%s' -> %A" title ex.Message
        Core.log ex
        Html.divc "is-danger" [ text <| string ex ]

type ButtonType =
    | Submit
    | Reset
    | Button

type ButtonClassArg =
    | Static of Choice<string, string seq>
    | Dynamic of System.IObservable<string>

let tButton title buttonClassArg (buttonType: ButtonType) props =
    let addButtonCls =
        function
        | ValueString cls -> $"button {cls}"
        | _ -> "button"

    Html.button [
        match buttonClassArg with
        | None -> Attr.className "button"
        | Some(Static(Choice1Of2 v)) -> addButtonCls v |> Attr.className
        | Some(Static(Choice2Of2 values)) -> Attr.classes [ "button"; yield! values ]
        | Some(Dynamic obs) -> Bind.attr ("class", obs |> Observable.map addButtonCls)

        type' (string buttonType |> String.toLower)
        Attr.title title
        yield! props
    ]

let bButton title props = tButton title None Button props
let bButtonC title bca props = tButton title (Some bca) Button props

let rButton title props = tButton title None Reset props

let columns2 col1 col2 =
    Html.divc "columns" [ Html.divc "column" col1; Html.divc "column" col2 ]

let columns3 col1 col2 col3 =
    Html.divc "columns" [ Html.divc "column" col1; Html.divc "column" col2; Html.divc "column" col3 ]


module Handlers =
    let debounceDefault = 400
    // change only fires like on focus lost for text input
    let onValueChange<'t> (dispatch: 't -> unit) f =
        Sutil.CoreElements.on "change" (Core.Handlers.getValue >> f >> dispatch) List.empty

    let onValueChangeIf<'t> (dispatch: 't -> unit) f =
        Sutil.CoreElements.on "change" (Core.Handlers.getValue >> f >> Option.iter dispatch) List.empty

    let onValueChangeIfD<'t> timeoutMs (dispatch: 't -> unit) f =
        onValueChangeIf<'t> (Core.debounce dispatch timeoutMs) f

    let onValueInput<'t> f =
        Sutil.CoreElements.on "input" (Core.Handlers.getValue >> f) List.empty

    let onValueInputD<'t> timeoutMs f =
        onValueInput<'t> (Core.debounce f timeoutMs)

// let onFocus f = on "focus" (fun _ -> f ())

// let onFocusable title isFocus fOnFocus =
//     if isFocus then
//         if focusTracing then
//             printfn "%s is focused" title

//         autofocus
//     else
//         onFocus fOnFocus []

type TextInputArgs = {
    Titling: string
    Value: System.IObservable<string>
    OnChange: string -> unit
    // unit allows for side effects
    // OnFocus: unit -> unit
    // IsFocus: bool
    DebounceOverride: int option
}

// assumes we won't use Attr.name for form field submission
let textInput (tia: TextInputArgs) children =
    Html.inputc "input" [
        type' "text"
        Bind.attr ("value", tia.Value)
        Attr.title tia.Titling
        Attr.placeholder tia.Titling
        // Handlers.onFocusable tia.Titling tia.IsFocus tia.OnFocus
        yield! children
        Handlers.onValueInputD (tia.DebounceOverride |> Option.defaultValue Handlers.debounceDefault) tia.OnChange
    ]

let checkbox titling (store: IReadOnlyStore<bool>) children (onChange: bool -> 't) (dispatch: Dispatch<'t>) =
    Html.inputc "checkbox" [
        type' "checkbox"
        // Attr.isChecked store.Value
        Bind.attr ("checked", value = store)
        Attr.title titling
        yield! children
        Handlers.onValueChange dispatch (fun _ -> not store.Value |> onChange)
    ]

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

type StoreOptions = {
    UseEquality: bool
    DebugTitle: string option
}

module Store =
    let mapRStore storeOptions (getter: 't -> 't2) (store: IReadOnlyStore<'t>) =
        // let mutable value = getter store.Value

        // debugTitleOpt
        // |> Option.iter (fun title -> printfn "%s: Initialized - %A" title value)

        { new IReadOnlyStore<'t2> with
            member _.Value = getter store.Value
            member _.Dispose() = store.Dispose()

            member _.Subscribe x =
                let f nextParentValue =
                    let nextValue = getter nextParentValue

                    match storeOptions.DebugTitle with
                    | None -> ()
                    | Some title -> printfn "RStore: %s - %A" title nextValue

                    nextValue |> x.OnNext

                if storeOptions.UseEquality then
                    store |> Observable.distinctUntilChanged |> Observable.subscribe f
                else
                    store.Subscribe f


        }

    let mapStore title (getter: 't -> 't2, setter: 't2 -> 't) (store: IStore<'t>) =
        let mutable name = store.Name + "." + title

        { new IStore<'t2> with
            member _.Debugger = store.Debugger

            member _.Subscribe x =
                store.Subscribe(fun value -> getter value |> x.OnNext)

            member _.Name
                with get () = name
                and set v = name <- v

            member _.Update f = store.Update(getter >> f >> setter)
            member _.Value = getter store.Value
            member _.Dispose() = store.Dispose()

        // Update=(fun oldValue -> setter oldValue)
        }

    let chooseStore title (getter, setter) init store =
        store |> mapStore title (getter >> Option.defaultValue init, Some >> setter)

    let chooseRStore storeOptions getter init store =
        store
        |> mapRStore storeOptions (fun parent -> getter parent |> Option.defaultValue init)
