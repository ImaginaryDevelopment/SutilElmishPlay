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

type IconEditorMsg = NameChange of propName:string * value:string

let renderIconEditor (propName, propObs) (value: string) (dispatch: Dispatch<IconEditorMsg>) =
    Html.div [
        tryIcon (App.Init.IconSearchType.MuiIcon value)
        formField [ text "Icon Name"] [
            Html.inputc "input" [
                type' "text"
                autofocus
                Attr.value value
                Handlers.onValueInput dispatch (fun v -> NameChange(propName,v))
            ]
            Html.divc "select" [
                Bind.el(propObs,
                    function
                    | ValueString iconPath ->
                        let toLower  = System.Char.ToLowerInvariant

                        let first = toLower iconPath.[0]
                        Html.select [
                            Attr.className "select" 
                            Handlers.onValueChangeIf dispatch (function | ValueString v -> NameChange(propName,v) |> Some | _ -> None)
                            Html.option [
                                text ""
                            ]
                            for o in Mui.all.Keys |> Seq.filter(fun k -> toLower k.[0] = first) do
                                Html.option [
                                    Attr.value o
                                    text o
                                    if iconPath = o then
                                        Attr.selected true
                                ]
                        ]
                    | _ -> Html.div []
                )
            ]
        ]

    ]

()