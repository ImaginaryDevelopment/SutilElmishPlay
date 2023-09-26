module App

open Browser.Types
open Fable.Core

open Sutil
open Sutil.CoreElements
open Adapters.Msal


type Model = {
    Counter : int
    AuthInfo: Result<AuthenticationResult,exn> option
}

[<RequireQualifiedAccess>]
type MsalMode =
    | Popup
    | Redirect

// Model helpers
let getCounter m = m.Counter
let getAuthInfo m = m.AuthInfo

let console = Browser.Dom.console
let window = Browser.Dom.window

type Message =
    | AuthFinished of Result<AuthenticationResult,exn>
    | Increment
    | Decrement

let msalMode = MsalMode.Redirect

let init () : Model * Cmd<Message> =
    let msalC =
        Msal.createConfig "" "" window.location.origin
        // |> Adapters.Msal.createPublicClientApplication
        // |> Adapters.Msal.PublicClientApplication.Create
        |> Adapters.Msal.PublicClientApplication
    console.log msalC

    let msalSetup () =
            promise {
                let! _ = msalC.initialize()
                printfn "Finished initialize"
                match msalMode with
                | MsalMode.Popup ->
                    let! ar = msalC.loginPopup(null)
                    printfn "Finished popup"
                    console.log ar
                    return ar
                | MsalMode.Redirect ->
                    console.log "Redirect Promise"
                    let! unk = msalC.handleRedirectPromise() 
                    console.log "unk"
                    console.log unk
                    match unk with
                    | null -> 
                        let! ar = msalC.loginRedirect(
                            {| scopes = [| "openid";"api://c9814d6d-c09a-4903-a776-533922c6c8fb/API.Access";
                                "user.readbasic.all" |] |})
                        console.log "ar"
                        console.log ar
                        invalidOp "Hello world"
                        return ar :?> AuthenticationResult
                    | v ->
                        return v :?> AuthenticationResult
            }

    { Counter = 0; AuthInfo = None }, Cmd.OfPromise.either msalSetup () (Ok >> AuthFinished) (Error>>AuthFinished)

let update (msg : Message) (model : Model) : Model * Cmd<Message> =
    match msg with
    | Increment -> { model with Counter = model.Counter + 1 }, Cmd.none
    | Decrement -> { model with Counter = model.Counter - 1 }, Cmd.none
    | AuthFinished x ->
        match x with
        | Error exn -> console.error exn
        | _ -> ()
        {model with AuthInfo = Some x}, Cmd.none

// In Sutil, the view() function is called *once*
let view() =

    // model is an IStore<ModeL>
    // This means we can write to it if we want, but when we're adopting
    // Elmish, we treat it like an IObservable<Model>
    let model, dispatch = () |> Store.makeElmish init update ignore
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

        Bind.el(model |> Store.map getAuthInfo, fun ai ->
            match ai with
            | None ->
                match msalMode with
                | MsalMode.Popup ->
                    Html.div [ text "Authorizing via popup..."]
                | MsalMode.Redirect ->
                    Html.div [ text "Checking authorization..."]
            | Some (Ok auth) ->
                Html.div [
                    text $"Welcome {auth.account.name}"
                ]
            | Some (Error exn) ->
                Html.div[
                    text "Failed auth"
                ]
        )

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
