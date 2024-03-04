module App.Components.IconEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
open App.Adapters
open App.Adapters.Html
open App.Adapters.Bulma

module Handlers = App.Adapters.Html.Handlers

open App.Adapters.Icons

open Core

type Model = {
    IconValue: IStore<string>
    SearchValue: string
    LastIsSelect: bool
    SearchClick: System.DateTime option
}

module MLens =
    let tryUpdateIconValue (store: IStore<string>) value =
        if value = "" || String.isValueString value then
            store.Update(fun _ -> value)

let init iconValue = {
    SearchValue = ""
    IconValue = iconValue
    LastIsSelect = true
    SearchClick = None
}

type Msg =
    | SearchChange of string
    | SearchIcons
    | SelectUsed of string

let update (msg: Msg) model =
    printfn "IconEditor update: %A" msg

    match msg with
    | SearchChange value -> { model with SearchValue = value }
    | SearchIcons ->
        if model.SearchValue |> String.length > 1 then
            {
                model with
                    LastIsSelect = false
                    SearchClick = Some System.DateTime.Now
            }
        else
            model
    | SelectUsed selectedName ->
        MLens.tryUpdateIconValue model.IconValue selectedName
        { model with LastIsSelect = true }

type IconEditorParentMsg = Accepted of value: string

let nameSelect (store: IReadOnlyStore<Model>) dispatch : SutilElement =
    match store.Value.SearchValue with
    | ValueString searchPath ->
        Html.divc "select" [
            printfn "Rerender select : %s" searchPath

            let options =
                if store.Value.LastIsSelect then
                    let toLower = System.Char.ToLowerInvariant

                    // not sure why this doesn't work with iconPath
                    let first = toLower searchPath[0]
                    Mui.all.Keys |> Seq.filter (fun k -> toLower k.[0] = first)
                else
                    let sv = store.Value.SearchValue.ToLowerInvariant()

                    Mui.all.Keys
                    |> Seq.filter (fun k -> not <| isNull k && k.ToLowerInvariant().Contains(sv))
                |> List.ofSeq

            Html.select [
                Attr.className "select"
                Handlers.onValueChangeIf dispatch (function
                    | ValueString v -> Some(SelectUsed v)
                    | _ -> None)
                Html.option [ text "" ]
                for o in options do
                    Html.option [
                        Attr.value o
                        if store.Value.IconValue.Value = o then
                            Attr.selected true
                        text o
                    ]
            ]
        ]
    | _ -> Html.div []


type IconEditorProps = { ValueStore: IStore<string> }

let renderIconEditor (props: IconEditorProps) (pDispatch: Dispatch<IconEditorParentMsg>) =
    let pDispatch msg =
        printfn "IconEditor Parent msg: %A" msg
        pDispatch msg

    toGlobalWindow "iconEditor_props" props
    let store, dispatch = props.ValueStore |> Store.makeElmishSimple init update ignore
    toGlobalWindow "iconEditor_model" store.Value

    let nameInput: SutilElement =

        textInput
            {
                Titling = "Name Input"
                Value = store.Value.IconValue
                OnChange = MLens.tryUpdateIconValue store.Value.IconValue
                DebounceOverride = None
            }
            []

    Html.div [
        Bind.el (store.Value.IconValue, (fun nv -> tryIcon (IconSearchType.MuiIcon nv)))

        // https://bulma.io/documentation/form/general/#form-addons
        formFieldAddons [] [
            textInput
                {
                    Titling = "Icon Search"
                    Value = store |> Store.map (fun v -> v.SearchValue)
                    OnChange = Msg.SearchChange >> dispatch
                    DebounceOverride = None
                }
                []
            bButton "Search" [
                // text "Search Icons"
                tryIcon (IconSearchType.MuiIcon "Search")
                onClick (fun _ -> Msg.SearchIcons |> dispatch) List.empty
            ]
        ] []

        formField [ text "Icon Name" ] [
            columns2 [] [ nameInput ] [
                Bind.el (store |> Store.map (fun v -> v.SearchValue), (fun _ -> nameSelect store dispatch))
            ]
        ] []


    ]
