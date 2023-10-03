module App.Adapters.Api

open BReusable
open Fetch
open Fetch.Types

open Fable.Core
open Fable.Core.JsInterop


open Browser

type MyInfoResponse = {
    ObjectID: string
    DisplayName: string
    FirstName: string
    LastName: string
    Email: string
    IsImperator: bool
}

type FetchArgs = {
    Token: string
    Arg: obj option // placeholder we'll want to be able to add to headers and such
    RelPath: string
}

// handle get/post + query params and/or json body
let fetch fetchArgs f =
    async{
        let! response =
            fetch (App.Adapters.Config.apiBase + fetchArgs.RelPath) [
                requestHeaders [
                    HttpRequestHeaders.Authorization $"Bearer %s{fetchArgs.Token}"
                ]] |> Async.AwaitPromise
        return! f response
    }
    |> Async.catch

let fetchText fetchArgs : Async<Result<_,exn>> =
    fetch fetchArgs (fun v -> v.text() |> Async.AwaitPromise)

let fetchJson<'t> tName fetchArgs : Async<Result<'t,exn>> =
    async {
        let! text = fetchText fetchArgs
        match text with
        | Ok text ->
            printfn "%s:Parsing" tName
            let parsed = Core.tryParse<'t> tName text
            return parsed
        | Error e ->
            return Error e
    }

let getMyInfo token: Async<Result<MyInfoResponse,exn>> =
    fetchJson<MyInfoResponse> "MyInfoResponse" {Token=token;RelPath="/api/profile/myInfo"; Arg=None}


// api/navigation/root
let getNavRoot token : Async<Result<obj,exn>> =
    fetchJson<obj> "obj" {Token=token;RelPath="api/navigation/root"; Arg=None}
