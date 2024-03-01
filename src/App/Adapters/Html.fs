module App.Adapters.Html

open BReusable

open Sutil
open Sutil.CoreElements

let (|StoreValue|) (x: Sutil.IStore<_>) = x.Value

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

let columns2 attrs col1 col2 =
    Html.divc "columns" [ yield! attrs; Html.divc "column" col1; Html.divc "column" col2 ]

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

// the existence of a selected attribute always means true it appears
type SelectType<'t> =
    | ObservedSelect of IReadOnlyStore<'t option> * onChange: ('t option -> unit)
    | ObservedMulti of IReadOnlyStore<'t list> * onChange: ('t -> unit)
    | StoredSelect of IStore<'t option>
    | StaticSelect of selected: 't option * valueMap: (string -> 't option) * onChange: ('t -> unit)

type SelectProps<'t> = {
    Values: 't seq
    HasEmpty: bool

    ValueGetter: 't -> string
    NameGetter: 't -> string

    SelectType: SelectType<'t>
    // for spreading attributes on children
    OptionChildren: 't option -> Core.SutilElement list
}

// no designed with multi-select in mind
// designed without looking at this https://sutil.dev/#examples-select-bindings?SelectBindings.fs
let selectInput (props: SelectProps<'t>) children =
    printfn "render selectInput"

    let tryFind rawId : 't option =
        props.Values
        |> Seq.tryFind (fun value -> props.ValueGetter value = rawId)
        |> function
            | None ->
                if not props.HasEmpty then
                    eprintfn "Selected option not found in values: %s" rawId

                None
            | Some value -> Some value

    let selectAttrs = [
        yield! children
        match props.SelectType with
        | ObservedSelect(store, onChange) ->
            yield! [
                Bind.attr ("data-store", store |> Store.map (Option.map props.ValueGetter))
                Handlers.onValueChange ignore (fun v ->
                    tryFind v
                    |> function
                        | None -> onChange None
                        | Some value -> onChange <| Some value)
            ]
        | StoredSelect store ->
            let updateStore value = store.Update(fun _ -> value)

            yield! [
                Bind.attr ("data-store", store |> Store.map (Option.map props.ValueGetter))
                Handlers.onValueChange updateStore (function
                    | ValueString selectedRawId -> tryFind selectedRawId
                    | _ -> None)
            ]
        | StaticSelect(_, valueMap, onChange) ->
            yield! [ Handlers.onValueChange ignore (valueMap >> Option.iter onChange) ]
        | ObservedMulti(store, onChange) ->
            printfn "reselect selectAttrs"

            yield! [
                Bind.attr (
                    "data-store",
                    store
                    |> Store.map (fun value ->
                        let nextValue = value |> List.map props.ValueGetter |> String.concat "|"
                        printfn "Multi will be '%s' <- %A" nextValue value
                        nextValue)
                )
                Handlers.onValueChange ignore (fun v ->
                    printfn "Multi-Value change: '%s'" v
                    v |> tryFind |> Option.iter onChange)
                Attr.multiple true
            ]

    ]

    Html.select [
        match props.SelectType with
        | ObservedMulti _ -> Attr.className "select select-multi is-multiple"
        | _ -> Attr.className "select"
        yield! selectAttrs

        if props.HasEmpty then
            Html.option [ text ""; yield! props.OptionChildren None ]
        printfn "rerender select options"
        for item in props.Values do
            let strId = props.ValueGetter item

            match props.SelectType with
            | ObservedSelect(store, _) ->
                Bind.el (
                    store,
                    fun selected ->
                        Html.option [
                            // data_ "item" (Core.pretty item.NavItem)
                            Attr.value strId
                            text <| props.NameGetter item
                            match selected with
                            | None -> ()
                            | Some selected ->
                                if props.ValueGetter selected = strId then
                                    Attr.selected true
                        ]
                )
            | StoredSelect store ->
                Bind.el (
                    store,
                    fun selected ->
                        Html.option[Attr.value strId
                                    text <| props.NameGetter item

                                    match selected with
                                    | None -> ()
                                    | Some selected ->
                                        if props.ValueGetter selected = strId then
                                            Attr.selected true]
                )
            | ObservedMulti(store, _) ->
                Bind.el (
                    store,
                    fun selected ->
                        Html.option[Attr.value strId
                                    text <| props.NameGetter item

                                    if selected |> List.map props.ValueGetter |> List.contains strId then
                                        Attr.selected true]
                )
            | StaticSelect(selected, _valueMap, _onChange) ->
                Html.option[Attr.value strId
                            text <| props.NameGetter item

                            match selected with
                            | None -> ()
                            | Some selected ->
                                if props.ValueGetter selected = strId then
                                    Attr.selected true]
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

    // should this be updated to distinct until changed
    let mapStore title useEquality (getter: 't -> 't2, setter: 't2 -> 't) (store: IStore<'t>) =
        let mutable name = store.Name + "." + title

        { new IStore<'t2> with
            member _.Debugger = store.Debugger

            member _.Subscribe x =
                if useEquality then
                    store |> Observable.distinctUntilChanged
                else
                    store
                |> Observable.subscribe (getter >> x.OnNext)

            member _.Name
                with get () = name
                and set v = name <- v

            member _.Update f = store.Update(getter >> f >> setter)
            member _.Value = getter store.Value
            member _.Dispose() = store.Dispose()

        // Update=(fun oldValue -> setter oldValue)
        }

    let chooseStore title useEquality (getter, setter) init store =
        store
        |> mapStore title useEquality (getter >> Option.defaultValue init, Some >> setter)

    let chooseRStore storeOptions getter init store =
        store
        |> mapRStore storeOptions (fun parent -> getter parent |> Option.defaultValue init)
