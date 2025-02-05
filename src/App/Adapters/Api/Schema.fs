module App.Adapters.Api.Schema
// 1. Schema types that might be tied directly to the api, but don't need to be mapped
// 2. Raw Module: Schema types that are tied directly to the api, and should be relegated to mapped interactions outside the api adapter

open BReusable

open Fable.Core

open App.Schema

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

type NavItemAclRefsMap = Map<AclName, Set<string>>

type NavItem = {
    Id: NavId
    // do we need path and parent?
    Path: string
    Parent: string
    Type: NavItemType
    Name: string
    DisplayName: string
    Description: string
    Icon: string
    Weight: int
    Enabled: bool
    Pinned: bool
    Url: string
    HasUrlKey: bool
    Managers: Set<AclRefId>
    Hash: string
    // this is poorly named if not all Acls are reference types
    // can't use Map<AclName, AclRefId>, could be param, or ref param
    [<CompiledName("Acls")>]
    AclRefs: NavItemAclRefsMap
} with

    static member IsNew(x: NavItem) = isNull (box x.Id) || x.Id = NavId ""

    static member CalcHash(x: NavItem) =
        let text = Core.serialize { x with Hash = null }
        let result = text.GetHashCode() |> string
        result

    static member SetHash(x: NavItem) =
        let nextHash = NavItem.CalcHash x
        { x with Hash = nextHash }
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

    // parent field may be ignore, but might be useful on our side for building path as name changes
    static member CreateEmpty parent = {

        Id = NavId null

        AclRefs = Map.empty
        Description = null
        DisplayName = ""
        Enabled = false
        HasUrlKey = false
        Hash = null
        Icon = "City"
        Managers = Set.empty
        Name = ""
        Parent = parent |> Option.defaultValue null
        Path = null
        Pinned = false
        Type = NavItemType.Folder
        Url = ""
        Weight = 0
    }

    static member GetName(navItem: NavItem) =

        navItem.DisplayName
        |> Option.ofValueString
        |> Option.orElseWith (fun _ -> Option.ofValueString navItem.Name)
        |> Option.defaultValue ""

    static member GetId(navItem: NavItem) =
        Option.ofUnsafe navItem
        |> Option.bind (fun navItem ->
            navItem.Id
            |> function
                | NavId nid -> nid |> Option.ofValueString)
        |> Option.defaultValue ""


    static member SetName value (navItem: NavItem) =
        if NavItem.IsNew navItem then
            { navItem with Name = value }
        else
            { navItem with DisplayName = value }


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

// based on CoreLib.Exceptions.CoreApiError
type CoreApiError = {
    Message: string
    StatusCode: int option
    StatusMessage: string
    ExceptionType: string
    ExceptionMessage: string
}

type NavAclResolveErrorResponse = {
    Reference: string
    DisplayName: string
    AdditionalInfo: obj
    Error: CoreApiError
}


// for bulk resolve, could be both some resolved and some errors
type NavAclResolveResponse = {
    Resolved: AclDisplay option
    Errors: NavAclResolveErrorResponse[] option
}

type NavAclsResolveResponse = {
    Resolved: AclDisplay[]
    Errors: NavAclResolveErrorResponse[] option
}

type AdminPickerBulkResolveResponse = {
    Managers: AclRefId[]
    Resolved: AclDisplay[]
    Errors: NavAclResolveErrorResponse[] option
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
        DisplayName: string option
        Description: string
        Icon: string
        Weight: int
        Enabled: bool option
        Pinned: bool option
        Url: string
        HasUrlKey: bool
        // [<CompiledName("Acls")>]
        Acls: ApiAclRef[]
        Managers: string[]
    } with

        member x.AclNames = x.Acls |> Seq.map (fun acl -> acl.Name)
