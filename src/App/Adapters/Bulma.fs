module App.Adapters.Bulma

open Sutil
open Sutil.CoreElements

open App.Adapters.Html

// https://bulma.io/documentation/form/general/
let formField labelContent controlContent errorContent =
    Html.divc "field" [
        Html.labelc "label" labelContent
        Html.divc "control" controlContent
        Html.p [ yield! errorContent ]
    ]

type TabType<'t> =
    | Enabled of 't
    | Disabled of className: string

type BulmaTab<'t> = {
    Name: string
    TabType: TabType<'t>
    IsActive: bool
    Render: unit -> Core.SutilElement
}

let renderTabs containerClasses items dispatch =
    let isActiveCn = "is-active"
    // Attr.className
    Html.div [
        Attr.classes [ "tabContainer"; yield! containerClasses ]
        data_ "file" "Bulma"
        data_ "method" "renderTabs"
        Html.divc "tabs" [
            Html.ul [
                for item in items do
                    Html.li [
                        match item.TabType with
                        | Disabled cn ->
                            Html.button [
                                Attr.title "No Item selected"
                                Attr.disabled true
                                text item.Name
                                Attr.className cn
                            ]
                        | Enabled msg ->
                            Html.a [
                                match item.IsActive with
                                | false -> onClick (fun _ -> dispatch msg) List.empty
                                | true -> Attr.classes [ isActiveCn; "has-text-primary" ]

                                text item.Name
                            ]
                    ]
            ]
        ]
        match items |> Seq.tryFind (fun item -> item.IsActive) with
        | Some active -> active.Render()
        | _ -> Html.div []
    ]
