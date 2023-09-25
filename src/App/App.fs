module App

open Browser.Types
open Sutil
open Sutil.CoreElements


type Model = { Counter : int }

// Model helpers
let getCounter m = m.Counter

let console = Browser.Dom.console
let window = Browser.Dom.window

type Message =
    | Increment
    | Decrement

let init () : Model =

    let msalC =
        Msal.createConfig "" ""
        // |> Adapters.Msal.createPublicClientApplication
        // |> Adapters.Msal.PublicClientApplication.Create
        |> Adapters.Msal.PublicClientApplication
    console.log msalC

    msalC.initialize().``then``(fun x ->
        printfn "Finished initialize"
        console.log x

        msalC.loginPopup(null).``then``(fun v ->
            printfn "Finished popup"
            console.log v
        )

    )
    { Counter = 0 }

let update (msg : Message) (model : Model) : Model =
    match msg with
    |Increment -> { model with Counter = model.Counter + 1 }
    |Decrement -> { model with Counter = model.Counter - 1 }

// In Sutil, the view() function is called *once*
let view() =

    // model is an IStore<ModeL>
    // This means we can write to it if we want, but when we're adopting
    // Elmish, we treat it like an IObservable<Model>
    let model, dispatch = () |> Store.makeElmishSimple init update ignore

    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]

        // See Sutil.Styling for more advanced styling options
        Attr.style [
            Css.fontFamily "Arial, Helvetica, sans-serif"
            Css.margin 20
        ]

        // Think of this line as
        // text $"Counter = {model.counter}"
        Bind.el (model |> Store.map getCounter, fun n ->
            text $"Counter = {n}" )

        Html.div [
            Html.button [
                Attr.className "button" // Bulma styling, included in index.html

                // Dispatching is as for normal ELmish. Sutil event handlers take an extra options array though
                Ev.onClick (fun _ -> dispatch Decrement)
                text "-"
            ]

            Html.button [
                Attr.className "button"
                Ev.onClick (fun _ -> dispatch Increment)
                text "+"
            ]
        ]]

// Start the app
view() |> Program.mount
