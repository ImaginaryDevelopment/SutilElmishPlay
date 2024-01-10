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

let private authStorage: Core.LocalStorage.IAccessor<AuthArgs> =
    Core.LocalStorage.StorageAccess<_>("authConfig")

let authConfig =
    let value = // value
        {
            AppGuid = null
            AppAuth = null
            ApiScope = null
            ApiDomainHint = null
            ApiBase = null
        }

    match authStorage.TryGetValue() with
    | None ->
        Core.toGlobalWindow "authConfig" value
        None
    | Some v -> Some v

    |> Option.defaultValue value
