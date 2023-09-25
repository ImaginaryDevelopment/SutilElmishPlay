module Adapters.Msal

open Fable.Core
open Fable.Core.JS

// sign in code example: https://learn.microsoft.com/en-us/azure/active-directory/develop/scenario-spa-sign-in?tabs=javascript2

// [<Emit("new PublicClientApplication($1)")>]
// let createPublicClientApplication (msalConfig: obj) : obj = jsNative

// let createPublicClientApplication (msalConfig: obj) : obj = import ""

// importAll "@azure/msal-browser"
// let publicClientApplication = import "PublicClientApplication" "@azure/msal-browser"
// type PublicClientApplication =
//     [<Emit("new PublicClientApplication($1)")>]
//     static member Create (config : obj) : PublicClientApplication = jsNative
type ILoginResponse =
    abstract account: obj
[<Import("PublicClientApplication", from="@azure/msal-browser")>]
type PublicClientApplication(conf:obj)=
    class
        member _.initialize() : Promise<obj> = jsNative // appears to return an option type?
        member _.loginPopup (reqConfig:obj) : Promise<ILoginResponse> = jsNative
    end
