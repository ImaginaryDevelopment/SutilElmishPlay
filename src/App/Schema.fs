module App.Adapters.Schema

open Fable.Core

type ErrorType = Choice<string[], exn>

[<Erase>]
type AclName =
    | AclName of string // Allow-By-Group, Allow-By-User, etc...

    static member getText =
        function
        | AclName name -> name

// AclParameter Id or RefId (we're reusing ref ids for non-reference type acl params it seems)
[<Erase>]
type AclRefId =
    | AclRefId of string

    static member getText =
        function
        | AclRefId name -> name

[<Erase>]
type NavId = NavId of string

module NavIds =
    let isValid (NavId x) =
        System.String.IsNullOrWhiteSpace x |> not

[<Erase>]
type AclReferenceType =
    | Group
    | User

type AclDisplay = {
    Reference: AclRefId
    DisplayName: string
    // group or user
    Type: AclReferenceType
    AdditionalInfo: obj option
}

type SaveType =
    | Create
    | Update

type AclLookup<'t> = Map<AclName, Map<AclRefId, 't>>

type ResolvedAclLookup = AclLookup<AclDisplay>
