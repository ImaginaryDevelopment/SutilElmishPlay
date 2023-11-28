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

type IconEditorMsg =
    | NameChange of propName: string * value: string
    | GotFocus

type IconEditorProps = {
    PropName: string
    PropObserver: System.IObservable<string>
    PropValue: string
    IsFocus: bool
}

let renderIconEditor (props: IconEditorProps) (dispatch: Dispatch<IconEditorMsg>) =
    toGlobalWindow "iconEditor_props" props

    let nameInput =
        Html.inputc "input" [
            type' "text"
            if props.IsFocus then
                autofocus
            else
                on "focus" (fun _ -> GotFocus |> dispatch) []
            Attr.value props.PropValue
            Handlers.onValueInputD 300 dispatch (fun v -> NameChange(props.PropName, v))
        ]

    let nameSelect =
        Bind.el (
            props.PropObserver,
            function
            | ValueString iconPath when props.PropValue.Length > 0 ->
                Html.divc "select" [
                    printfn "Rerender select : %s" iconPath

                    let toLower = System.Char.ToLowerInvariant

                    // not sure why this doesn't work with iconPath
                    let first = toLower props.PropValue[0]

                    Html.select [
                        Attr.className "select"
                        Handlers.onValueChangeIf dispatch (function
                            | ValueString v -> NameChange(props.PropName, v) |> Some
                            | _ -> None)
                        Html.option [ text "" ]
                        for o in Mui.all.Keys |> Seq.filter (fun k -> toLower k.[0] = first) do
                            Html.option [
                                Attr.value o
                                text o
                                if iconPath = o then
                                    Attr.selected true
                            ]
                    ]
                ]
            | _ -> Html.div []
        )

    Html.div [
        tryIcon (App.Init.IconSearchType.MuiIcon props.PropValue)
        formField [ text "Icon Name" ] [ columns2 (Seq.ofList [ nameInput ]) (Seq.ofList [ nameSelect ]) ] []

    ]

()
