module App.Components.IconEditor

open BReusable

open Sutil
open Sutil.CoreElements

open App.Init
open App.Adapters
open App.Adapters.Html
open App.Adapters.Bulma

module Handlers = App.Adapters.Html.Handlers

open App.Components.Gen.Icons

open Core

type Model = {
    SearchValue: string
    LastIsSelect: bool
    SearchClick: System.DateTime option
}

let init () = {
    SearchValue = ""
    LastIsSelect = true
    SearchClick = None
}

type Msg =
    | SearchChange of string
    | SearchIcons
    | SelectUsed

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
    | SelectUsed -> { model with LastIsSelect = true }

type IconEditorParentMsg =
    | NameChange of propName: string * value: string
    | GotFocus

type IconEditorProps = {
    PropName: string
    PropObserver: System.IObservable<string>
    PropValue: string
    IsFocus: bool
}

let renderIconEditor (props: IconEditorProps) (pDispatch: Dispatch<IconEditorParentMsg>) =
    let pDispatch msg =
        printfn "IconEditor Parent msg: %A" msg
        pDispatch msg

    toGlobalWindow "iconEditor_props" props
    let store, dispatch = () |> Store.makeElmishSimple init update ignore
    toGlobalWindow "iconEditor_model" store.Value

    let nameInput: SutilElement =
        Html.inputc "input" [
            type' "text"
            if props.IsFocus then
                autofocus
            else
                on "focus" (fun _ -> GotFocus |> pDispatch) []
            Attr.value props.PropValue
            Handlers.onValueInputD 300 pDispatch (fun v ->
                dispatch SelectUsed
                NameChange(props.PropName, v))
        ]

    // TODO: name select search
    let nameSelect model : SutilElement =
        Bind.el (
            props.PropObserver,
            function
            | ValueString iconPath when
                props.PropValue.Length > 0
                || (not model.LastIsSelect && model.SearchValue.Length > 0)
                ->
                Html.divc "select" [
                    printfn "Rerender select : %s" iconPath

                    let options =
                        if model.LastIsSelect then
                            let toLower = System.Char.ToLowerInvariant

                            // not sure why this doesn't work with iconPath
                            let first = toLower props.PropValue[0]
                            Mui.all.Keys |> Seq.filter (fun k -> toLower k.[0] = first)
                        else
                            let sv = model.SearchValue.ToLowerInvariant()

                            Mui.all.Keys
                            |> Seq.filter (fun k -> not <| isNull k && k.ToLowerInvariant().Contains(sv))
                        |> List.ofSeq

                    Html.select [
                        Attr.className "select"
                        Handlers.onValueChangeIf pDispatch (function
                            | ValueString v ->
                                dispatch SelectUsed
                                NameChange(props.PropName, v) |> Some
                            | _ -> None)
                        Html.option [ text "" ]
                        for o in options do
                            Html.option [
                                Attr.value o
                                if iconPath = o then
                                    Attr.selected true
                                text o
                            ]
                    ]
                ]
            | _ -> Html.div []
        )

    Html.div [
        tryIcon (App.Init.IconSearchType.MuiIcon props.PropValue)

        formField [ text "Search" ] [
            textInput "Icon Search" store.Value.SearchValue [] (fun v -> Msg.SearchChange v) dispatch
            bButton "Search" [
                text "Search Icons"
                onClick (fun _ -> Msg.SearchIcons |> dispatch) List.empty
            ]
        ] []
        formField [ text "Icon Name" ] [
            columns2 [ nameInput ] [
                Bind.el2
                    (store |> Store.map (fun model -> model.LastIsSelect))
                    (store |> Store.map (fun model -> model.SearchClick))
                    (fun _ -> nameSelect store.Value)
            ]
        ] []

    ]
