module App.Adapters.Bulma

open Sutil
open Sutil.CoreElements

open App.Adapters.Icons
open App.Adapters.Html

// https://bulma.io/documentation/form/general/
let formField labelContent controlContent (errorContent: Core.SutilElement list) =
    Html.divc "field" [
        Html.labelc "label" labelContent
        Html.divc "control" controlContent
        Html.p errorContent
    ]

// https://bulma.io/documentation/form/general/#form-addons
let formFieldAddons labelContent (controls: Core.SutilElement List) (errorContent: Core.SutilElement list) =
    Html.divc "field has-addons" [
        Html.labelc "label" labelContent
        yield! controls |> List.map (fun c -> Html.divc "control" [ c ])
        Html.p errorContent
    ]

[<RequireQualifiedAccess>]
type CardContentType =
    | Header of Core.SutilElement
    // is it recommended you use is-4by3 and a figure or semi-required?
    | Image of Core.SutilElement
    | Content of Core.SutilElement list
    | Footer of footerItems: Core.SutilElement list


let renderCardContentType =
    function
    | CardContentType.Header child -> Html.headerc "card-header" [ Html.divc "card-header-title" [ child ] ]
    | CardContentType.Image child -> Html.divc "card-image" [ child ]
    | CardContentType.Content children -> Html.divc "card-content" children
    | CardContentType.Footer children ->
        Html.divc "card-footer" (children |> List.map (fun child -> Html.divc "card-footer-item" [ child ]))

let card contents =
    Html.divc "card" [
        yield! contents |> Seq.map renderCardContentType

    ]

let collapsibleCard storeOpt header children =
    let store =
        storeOpt
        |> Option.map (function
            | Choice1Of2 storeOverride -> storeOverride
            | Choice2Of2 initialValue -> initialValue |> Store.make)
        |> Option.defaultWith (fun () -> false |> Store.make)

    Html.divc "card" [
        Html.headerc "card-header" [
            Html.divc "card-header-title" [ header ]

            Html.buttonc "card-header-icon" [
                onClick (fun _ -> store.Update(fun _ -> not store.Value)) []
                Html.spanc "icon" [
                    Bind.el (
                        store,
                        function
                        | true -> "ExpandLess"
                        | false -> "ExpandMore"
                        >> IconSearchType.MuiIcon
                        >> tryIcon
                    )

                ]
            // Html.spanc "icon" [

            //     Html.i [
            //         Bind.classNames (
            //             store
            //             |> Observable.map (function
            //                 | false -> [ "fas"; "fa-angle-down" ]
            //                 | true -> [ "fas"; "fa-angle-up" ])
            //         )
            //     ]
            // ]
            ]
        ]

        Bind.el (
            store |> Store.map id,
            (fun v ->
                if v then
                    fragment (children |> List.map renderCardContentType)
                else
                    Html.div [])
        )
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
