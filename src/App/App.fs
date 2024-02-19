module AppMain

open BReusable

open Browser.Types
open Fable.Core

open Sutil
open Sutil.CoreElements

open App.Adapters
open App.Adapters.Config
open App.Adapters.Msal
open App.Components.Gen

module Css =

    open Sutil.Styling
    open type Feliz.length

    let globalCss = [

        rule "span.info" CssRules.titleIndicator
        rule ":global(.tabContainer)" [ Css.width (percent 100); Css.backgroundColor ("black") ]
        rule ".fill" [ Css.width (percent 100) ]


        rule "div.iconColumn" [ Css.height (em 1.0); Css.width (em 1.0); Css.flexShrink 0 ]
        rule "div.buttonColumn" [ Css.height (em 1.0); Css.width (em 2.5); Css.flexShrink 0 ]
    // rule ".tile .field" [ Css.marginRight (px 5) ]
    // rule ".tile .field .control .box" [ Css.minWidth (px 450) ]
    // rule "div.buttonColumn button.button" [ Css.fontSize (em 0.7) ]
    ]

[<RequireQualifiedAccess>]
type MsalMode =
    | Popup
    | Redirect

let msalMode = MsalMode.Redirect

type Model = {
    Title: string
    AppMode: App.Adapters.Config.ConfigType<AuthArgs>
    AuthInfo: Result<AuthenticationResult * TokenRequestResult, exn> option
}

// Model helpers
module MLens =
    let getAuthInfo m = m.AuthInfo

let console = Browser.Dom.console
let window = Browser.Dom.window

type Message = AuthFinished of Result<AuthenticationResult * TokenRequestResult, exn>

module Commands =

    let setupMsal () =

        let msalC =

            Msal.createConfig App.Adapters.Config.authConfig.AppGuid Config.authConfig.AppAuth window.location.origin
            // |> Adapters.Msal.createPublicClientApplication
            // |> Adapters.Msal.PublicClientApplication.Create
            |> Msal.PublicClientApplication
        // console.log msalC

        let msalSetup () =
            promise {
                let! _ = msalC.initialize ()
                printfn "Finished initialize"

                let! ar =
                    promise {
                        match msalMode with
                        | MsalMode.Popup ->
                            let! ar = msalC.loginPopup (null)
                            printfn "Finished popup"
                            console.log ar
                            return ar
                        | MsalMode.Redirect ->
                            console.log "Redirect Promise"
                            let! unk = msalC.handleRedirectPromise ()
                            console.log "unk"
                            console.log unk

                            match unk with
                            | null ->
                                let! ar =
                                    msalC.loginRedirect (
                                        {|
                                            scopes = [| "openid"; Config.authConfig.ApiScope; "user.readbasic.all" |]
                                            extraQueryParameters = {|
                                                domain_hint = Config.authConfig.ApiDomainHint
                                            |}
                                        |}
                                    )

                                console.log "ar"
                                console.log ar
                                invalidOp "Hello world"
                                return ar
                            | v -> return v :?> AuthenticationResult
                    }

                match msalC.getAllAccounts () |> List.ofArray with
                | [] ->
                    eprintfn "No Accounts found"
                    return invalidOp "No accounts found"
                | h :: _ ->
                    let! token = msalC.acquireTokenSilent ({| account = h |})
                    return (ar, token)
            }

        Cmd.OfPromise.either msalSetup () (Ok >> AuthFinished) (Error >> AuthFinished)


// consider https://stackoverflow.com/questions/68970069/how-to-make-svelte-re-use-a-component-instance-somewhere-else-in-the-dom-instead
// let mutable components = null

let mustAuthEl title (model: IStore<Model>) f =
    match model.Value.AppMode with
    | Demo ->
        let render = App.Adapters.Html.tryRender title f Demo
        Html.div [ render ]
    | ConfigType.Auth _ ->
        Bind.el (
            model |> Store.map MLens.getAuthInfo,
            fun ai ->
                ai
                |> App.Adapters.Html.tryRender title (function
                    | Some(Ok auth) -> Html.div [ f (Auth auth) ]
                    | Some(Error exn) -> Html.pre [ text (string exn) ]
                    | None -> Html.div [])
        )

// make sure the module loads and is not tree-shaken out
App.Adapters.Mui.all |> ignore


let init () : Model * Cmd<Message> =

    let title, appMode, cmd =
        match App.Adapters.Config.authConfig.AppGuid, Config.authConfig.AppAuth with
        | NonValueString, NonValueString -> "App Demo", App.Adapters.Config.ConfigType.Demo, Cmd.none
        | NonValueString, _ -> failwith "Missing App Guid"
        | _, NonValueString -> failwith "Missing AppAuth"
        | ValueString _, ValueString _ ->
            "", App.Adapters.Config.ConfigType.Auth Config.authConfig, Commands.setupMsal () // Cmd.OfPromise.either msalSetup () (Ok >> AuthFinished) (Error >> AuthFinished)

    {
        AuthInfo = None
        Title = title
        AppMode = appMode
    },
    cmd

let update (msg: Message) (model: Model) : Model * Cmd<Message> =
    printfn "AdminExplorer msg update"

    match msg with
    | AuthFinished x ->
        match x with
        | Error exn -> console.error exn
        | _ -> ()

        { model with AuthInfo = Some x }, Cmd.none

open Css
// In Sutil, the view() function is called *once*
let view () =

    // model is an IStore<ModeL>
    // This means we can write to it if we want, but when we're adopting
    // Elmish, we treat it like an IObservable<Model>
    let model, dispatch = () |> Store.makeElmish init update ignore

    let tabStore =
        let tabParent = {
            Name = "AppRoot"
            ActiveTab = 0
            Tabs = [|
                {
                    Label = "Explorer"
                    Value = 0
                    Component =
                        mustAuthEl "ExplorerTab" model (function
                            | Auth(ai, token) -> App.Components.Admin.Explorer.view token.accessToken
                            | Demo -> App.Components.Admin.Explorer.Samples.view ())
                }
                {
                    Label = "Admin"
                    Value = 1
                    Component =
                        mustAuthEl "AdminTab" model (function
                            | Auth(ai, token) -> App.Components.Root.view (Auth token.accessToken)
                            | Demo -> App.Components.Root.view Demo)
                }
                {
                    Label = "Diag"
                    Value = 2
                    Component =
                        mustAuthEl "DiagTab" model (function
                            | Auth(ai, token) -> App.Components.Diag.view { AppMode = Auth token.accessToken }
                            | Demo -> App.Components.Diag.view { AppMode = Demo })
                }
            |]
        }

        () |> Store.makeElmishSimple (fun _ -> tabParent) Tabs.update ignore

    Html.div [
        // Get used to doing this for components, even though this is a top-level app.
        disposeOnUnmount [ model ]
        text model.Value.Title
        // See Sutil.Styling for more advanced styling options
        Attr.style [ Css.fontFamily "Arial, Helvetica, sans-serif"; Css.margin 20 ]

        Bind.el (
            model |> Store.map MLens.getAuthInfo,
            fun ai ->
                match ai, model.Value.AppMode with
                | _, Demo -> Html.div [] // App.Components.Root.view ConfigType.Demo
                | None, _ ->
                    match msalMode with
                    | MsalMode.Popup -> Html.div [ text "Authorizing via popup..." ]
                    | MsalMode.Redirect -> Html.div [ text "Checking authorization..." ]
                | Some(Ok(auth, token)), _ -> Html.div [ text $"Welcome {auth.account.name}" ]
                | Some(Error exn), _ -> Html.div[text "Failed auth"]
        )
        mustAuthEl "tabStore" model (fun _ -> Html.div [ App.Components.Gen.Tabs.view (Choice2Of2 tabStore) ])

    ]

let _removeMySheet =
    globalCss |> Sutil.Styling.addGlobalStyleSheet (Browser.Dom.document)

App.Init.FA.dom |> ignore
// Start the app
view () |> Program.mount
