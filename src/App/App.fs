module App

open Browser.Types
open Fable.Core

open Sutil
open Sutil.CoreElements

open App.Adapters
open App.Adapters.Msal
open App.Components.Gen

type Model = {
    Counter : int
    AuthInfo: Result<AuthenticationResult*TokenRequestResult,exn> option
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
    | AuthFinished of Result<AuthenticationResult*TokenRequestResult,exn>
    | Increment
    | Decrement

let msalMode = MsalMode.Redirect

// consider https://stackoverflow.com/questions/68970069/how-to-make-svelte-re-use-a-component-instance-somewhere-else-in-the-dom-instead
// let mutable components = null

let mustAuthEl model f =
    Bind.el(model |> Store.map getAuthInfo, fun ai ->
        match ai with
        | Some (Ok auth) ->
            Html.div [ f auth ]
        | Some (Error exn) ->
            Html.pre [ text (string exn)]
        | None -> Html.div []
)

let init () : Model * Cmd<Message> =

    let msalC =
        Msal.createConfig App.Adapters.Config.appGuid Config.appAuth window.location.origin
        // |> Adapters.Msal.createPublicClientApplication
        // |> Adapters.Msal.PublicClientApplication.Create
        |> Msal.PublicClientApplication
    console.log msalC

    let msalSetup () =
            promise {
                let! _ = msalC.initialize()
                printfn "Finished initialize"
                let! ar =
                    promise {
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
                                    {|
                                        scopes = [| "openid"; Config.apiScope ;
                                        "user.readbasic.all" |]
                                        extraQueryParameters = {| domain_hint= Config.apiDomainHint |}|})
                                console.log "ar"
                                console.log ar
                                invalidOp "Hello world"
                                return ar
                            | v ->
                                return v :?> AuthenticationResult
                    }
                match msalC.getAllAccounts() |> List.ofArray with
                | [] ->
                    eprintfn "No Accounts found"
                    return invalidOp "No accounts found"
                | h :: _ -> 
                    let! token = msalC.acquireTokenSilent({| account= h|})
                    return (ar,token)
            }
    { Counter = 0; AuthInfo = None;}, Cmd.OfPromise.either msalSetup () (Ok >> AuthFinished) (Error>>AuthFinished)

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
    let tabStore =
        let tabParent =
            {
                ActiveTab= 0
                Tabs = [|
                    {
                        Label="Root"
                        Value=0
                        Component= 
                            mustAuthEl model (fun (ai,token) ->
                                App.Components.Root.view token.accessToken

                            )
                    }
                    {
                        Label= "Diag"
                        Value= 1
                        Component =
                            mustAuthEl model (fun (ai,token) ->
                                App.Components.Diag.view token.accessToken
                            )
                    }

                |]
            }
        () |> Store.makeElmishSimple (fun _ -> tabParent) Tabs.update ignore

    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]

        // See Sutil.Styling for more advanced styling options
        Attr.style [
            Css.fontFamily "Arial, Helvetica, sans-serif"
            Css.margin 20
        ]
        let mustAuthEl f =
            Bind.el(model |> Store.map getAuthInfo, fun ai ->
                match ai with
                | Some (Ok auth) ->
                    Html.div [ f auth ]
                | Some (Error exn) ->
                    Html.pre [ text (string exn)]
                | None -> Html.div []
            )

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
            | Some (Ok (auth,token)) ->
                Html.div [
                    text $"Welcome {auth.account.name}"
                ]
            | Some (Error exn) ->
                Html.div[
                    text "Failed auth"
                ]
        )
        mustAuthEl (fun (ai,token)->
            Html.div [
                text "Tabs?"
                App.Components.Gen.Tabs.view (Choice2Of2 tabStore)
            ]
        )
        // this element seems to get reset when the model changes
        mustAuthEl (fun (ai,token)->
            // text $"yay auth"
            Html.div[
                App.Components.Root.view token.accessToken
                App.Components.Diag.view token.accessToken
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
