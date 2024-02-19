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

type TabVariance<'t> =
    | NoVariance of TabType<'t>
    | Variance of IReadOnlyStore<TabType<'t>>

type BulmaTab<'t> = {
    Name: string
    TabType: TabVariance<'t>
    IsActive: IReadOnlyStore<bool>
    Value: Core.SutilElement
}

let private renderTab tt (item: BulmaTab<_>) dispatch =
    let isActiveCn = "is-active"

    match tt with
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

let renderTabs containerClasses items dispatch =
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
                        | NoVariance tt -> renderTab tt item dispatch
                        | Variance tt -> Bind.el (tt, (fun tt -> renderTab tt item dispatch))
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
