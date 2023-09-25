module Msal

let createConfig clientId authority origin =
    {|
        auth =
            {|
                clientId= clientId
                authority= authority
                redirectUri= origin
            |}
        cache =
            {|
                cacheLocation="sessionStorage"
                storeAuthStateInCookie= false
            |}
    |}

