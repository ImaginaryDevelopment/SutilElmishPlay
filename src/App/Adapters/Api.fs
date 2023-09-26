module App.Adapters.Api

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

// handle get/post + query params and/or json body
let fetch<'t> token relPath arg =
    async{
        let! response =
            fetch (App.Adapters.Config.apiBase + relPath) [
                requestHeaders [
                    HttpRequestHeaders.Authorization $"Bearer %s{token}"
                ]] |> Async.AwaitPromise
        return! response.text() |> Async.AwaitPromise
    }
    |> Async.Catch

let getMyInfo token: Async<Result<MyInfoResponse,exn>> =
    async {
        let! value = fetch<MyInfoResponse> token "/api/profile/myInfo" None
        match value with
        | Choice1Of2 v ->
            printfn "Parsing my info"
            return Core.tryParse<MyInfoResponse> "myInfoResponse" v
        | Choice2Of2 err -> return Error err
    }


