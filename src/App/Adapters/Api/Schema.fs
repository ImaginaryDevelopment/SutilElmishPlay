module App.Adapters.Api.Schema
// 1. Schema types that might be tied directly to the api, but don't need to be mapped
// 2. Raw Module: Schema types that are tied directly to the api, and should be relegated to mapped interactions outside the api adapter

open BReusable

open Fable.Core

open App.Adapters.Schema

[<StringEnum>]
type NavItemType =
    | [<CompiledName("Link")>] Link
    | [<CompiledName("Folder")>] Folder

    static member All = [ Link; Folder ]

    static member TryParse =
        function
        | y when y = string Link -> Some Link
        | y when y = string Folder -> Some Folder
        | _ -> None

type AclData = {
    Name: AclName
    // can't use AclRefId, could be param, or ref param
    Parameters: Set<string>
}

[<RequireQualifiedAccess>]
type AclParameterType =
    | None
    | Selectable of selectableParameters: string[]
    | Reference of searchable: bool

type AclType = {
    Name: AclName
    MultiValue: bool
    AclParamType: AclParameterType
}

type NavItem = {
    Id: NavId
    // do we need path and parent?
    Path: string
    Parent: string
    Type: NavItemType
    Name: string
    Description: string
    Icon: string
    Weight: int
    Enabled: bool
    Url: string
    HasUrlKey: bool
    // this is poorly named if not all Acls are reference types
    // can't use Map<AclName, AclRefId>, could be param, or ref param
    [<CompiledName("Acls")>]
    AclRefs: Map<AclName, Set<string>>
} with
    // value could be a literal value, or a reference
    static member TryChangeAclNameParam aclName (f: Set<_> -> Set<_>) (x: NavItem) =
        x.AclRefs
        |> Map.tryFind aclName
        |> function
            | None -> Error "AclName not present"
            | Some paramSet ->
                Ok(
                    {
                        x with
                            AclRefs = x.AclRefs |> Map.add aclName (f paramSet)
                    }
                )
    // refactor using the above method
    static member AddAclRefParam aclName value (x: NavItem) =
        x.AclRefs
        |> Map.tryFind aclName
        |> function
            | None -> Error "AclName not present"
            | Some paramSet ->
                Ok {
                    x with
                        AclRefs = x.AclRefs |> Map.add aclName (paramSet |> Set.add value)
                }

    //  filter down to only reference types
    static member GetRefParams (aclTypes: AclType seq) (x: NavItem) =
        x.AclRefs
        |> Map.choose (fun k v ->
            aclTypes
            |> Seq.tryFind (fun aclType -> aclType.Name = k)
            |> Option.bind (fun aclType ->
                match aclType.AclParamType with
                | AclParameterType.Reference _ -> Some(k, (aclType, v |> Set.map AclRefId))
                | _ -> None))

// I think the compiled name was needed for serialization/deserialization, it assumed camel case without it
[<RequireQualifiedAccess; StringEnum>]
type ApiAclParameterType =
    | [<CompiledName("None")>] None
    | [<CompiledName("Selectable")>] Selectable
    | [<CompiledName("Reference")>] Reference


type MyInfoResponse = {
    ObjectID: string
    DisplayName: string
    FirstName: string
    LastName: string
    Email: string
    IsImperator: bool
}



type ApiAclSearchResponse = {
    Search: string
    Results: AclDisplay[]
}

type AclSearchResult = {
    AclName: AclName
    Data: ApiAclSearchResponse
}

type NavAclResolveSubError = {
    Message: string
    StatusCode: int
    StatusMessage: string
    ExceptionType: string
    ExceptionMessage: string
}

type NavAclResolveErrorResponse = {
    Reference: string
    DisplayName: string
    AdditionalInfo: obj
    Error: NavAclResolveSubError
}

type NavAclResolveResponse = {
    Resolved: AclDisplay[]
    // ?
    Errors: NavAclResolveErrorResponse[]
}
// api types that we want to restrict to mapped interactions outside of the api adapter layer
module Raw =

    // name is AclName
    type ApiAclRef = {
        Name: string
        Parameters: string[]
    } with

        member x.AclName = AclName x.Name

    type ApiAcl = {
        Name: string // AclName: Allow-By-Group, Allow-By-User, etc...
        ParameterType: ApiAclParameterType // None, Selectable, Reference
        MultiValue: bool option
        Searchable: bool option
        SelectableParameters: string[] option
    } with

        member x.AclName = AclName x.Name

        member x.AclParameterType: AclParameterType =
            let sch = x.Searchable |> Option.defaultValue false

            match x.ParameterType with
            | ApiAclParameterType.None -> AclParameterType.None
            | ApiAclParameterType.Selectable ->
                AclParameterType.Selectable(x.SelectableParameters |> Option.defaultValue Array.empty)
            | ApiAclParameterType.Reference -> AclParameterType.Reference(sch)

        static member ToAclType(apiAcl: ApiAcl) : AclType = {
            AclParamType = apiAcl.AclParameterType
            MultiValue = apiAcl.MultiValue |> Option.defaultValue false
            Name = apiAcl.AclName
        }

    type ApiNavItem = {
        Id: string
        Path: string
        Parent: string
        Type: string
        Name: string
        Description: string
        Icon: string
        Weight: int
        Enabled: bool option
        Url: string
        HasUrlKey: bool
        // [<CompiledName("Acls")>]
        Acls: ApiAclRef[]
    } with

        member x.AclNames = x.Acls |> Seq.map (fun acl -> acl.Name)
