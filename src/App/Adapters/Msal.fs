module Adapters.Msal

open Fable.Core

// [<Emit("new PublicClientApplication($1)")>]
// let createPublicClientApplication (msalConfig: obj) : obj = jsNative

// let createPublicClientApplication (msalConfig: obj) : obj = import ""

// importAll "@azure/msal-browser"
// let publicClientApplication = import "PublicClientApplication" "@azure/msal-browser"
// type PublicClientApplication =
//     [<Emit("new PublicClientApplication($1)")>]
//     static member Create (config : obj) : PublicClientApplication = jsNative

[<Import("PublicClientApplication", from="@azure/msal-browser")>]
type PublicClientApplication(conf:obj)=
    class
    end
