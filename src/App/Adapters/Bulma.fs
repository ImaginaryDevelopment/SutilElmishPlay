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
    IsActive: IReadOnlyStore<bool>
    Value: Core.SutilElement
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
                            let aClsStore =
                                item.IsActive
                                |> Store.map (fun v -> if v then isActiveCn + " has-text-primary" else "")

                            Html.a [
                                Bind.attr ("class", aClsStore)
                                onClick
                                    (fun _ ->
                                        if not item.IsActive.Value then
                                            dispatch msg)
                                    List.empty
                                text item.Name
                            ]
                    ]
            ]
        ]

        yield!
            items
            |> Seq.map (fun bt ->
                Html.div [
                    Bind.attr ("class", bt.IsActive |> Store.map (fun v -> if not v then "is-hidden" else ""))
                    // if not bt.IsActive.Value then
                    //     Attr.className "is-invisible"
                    bt.Value
                ])
    // match items |> Seq.tryFind (fun item -> item.IsActive) with
    // | Some active -> active.Render()
    // | _ -> Html.div []
    ]
