module App.Adapters.Msal

open Fable.Core
open Fable.Core.JS

// sign in code example: https://learn.microsoft.com/en-us/azure/active-directory/develop/scenario-spa-sign-in?tabs=javascript2

[<Import("AccountEntity", from="@azure/msal-browser")>] // AccountInfo.ts
[<AbstractClass>]
type AccountInfo =
    abstract member authorityType: obj
    abstract member homeAccountId: obj
    abstract member localAccountId: obj
    abstract member nativeAccountId: obj
    abstract member realm: obj
    abstract member environment: obj
    abstract member username: obj
    abstract member name: obj
    abstract member idTokenClaims: obj
    abstract member cloudGraphHostName: obj
    abstract member msGraphHost: obj
    static member isAccountEntity: obj -> bool = jsNative


// export type AuthenticationResult = CommonAuthenticationResult & {
//     account: AccountInfo;
// };
type AuthenticationResult = // AuthenticationResult.ts
    abstract accessToken: string
    abstract account: AccountInfo
    abstract expiresOn: obj
    abstract extExpiresOn: obj
    abstract scopes: string[]
    abstract tokenType: string

type TokenRequestResult =
    abstract accessToken: string
    // new Date(res.expiresOn)
    abstract expiresOn: obj

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
        member _.initialize() : Promise<obj> = jsNative // appears to return an option type?
        member _.loginPopup (reqConfig:obj) : Promise<AuthenticationResult> = jsNative
        member _.loginRedirect(loginRequestConfig: obj) : Promise<AuthenticationResult> = jsNative
        member _.handleRedirectPromise() : Promise<obj> = jsNative
        member _.acquireTokenSilent(request:obj) : Promise<TokenRequestResult> = jsNative
        member _.getAllAccounts() : AccountInfo[] = jsNative
    end

// @azure/msal-browser - msal-browser.js