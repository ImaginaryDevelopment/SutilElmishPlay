module App.Adapters.Api

open Fetch
open Fetch.Types
open Fable.Core
open Fable.Core.JsInterop

// handle get/post + query params and/or json body
let fetch<'t> token relPath arg =
    async{
        let! response =
            fetch (App.Adapters.apiBase + relPath) [
                requestHeaders [
                    HttpRequestHeaders.Authorization $"Bearer %s{token}"
                ]] |> Async.AwaitPromise
        return response.text()
    }
    |> Async.Catch

let getMyInfo token =
    fetch<obj> token "/api/profile/myInfo" None


