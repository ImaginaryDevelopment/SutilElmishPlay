module App.Adapters.Bulma

open Sutil
open Sutil.CoreElements

// https://bulma.io/documentation/form/general/
let formField labelContent controlContent =
    Html.divc "field" [ Html.labelc "label" labelContent; Html.divc "control" controlContent ]

type BulmaTab<'t> = {
    Name: string
    TabClickMsg: 't
    IsActive: bool
    Render: unit -> Core.SutilElement
}

let tabs items dispatch =
    Html.div [
        Html.divc "tabs" [
            Html.ul [
                for item in items do
                    Html.li [
                        if item.IsActive then
                            Attr.className "is-active"
                        Html.a [
                            if not item.IsActive then
                                onClick (fun _ -> dispatch item.TabClickMsg) List.empty

                            text item.Name
                        ]
                    ]
            ]
        ]
        match items |> Seq.tryFind (fun item -> item.IsActive) with
        | Some active -> active.Render()
        | _ -> Html.div []
    ]
