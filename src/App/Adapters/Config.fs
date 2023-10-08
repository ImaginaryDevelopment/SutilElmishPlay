//cspell: ignore: duvalschools
module App.Adapters.Config

type AuthArgs = {
    AppGuid: string
    AppAuth: string
    ApiScope: string
    ApiDomainHint: string
    ApiBase: string
}

type ConfigType<'t> =
    | Auth of 't
    | Demo

let appGuid = ""
let appAuth = ""
let apiScope = ""
let apiDomainHint = ""
let apiBase = ""