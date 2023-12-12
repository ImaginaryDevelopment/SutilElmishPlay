module App.Adapters.Schema

type ErrorType = Choice<string[], exn>

type AclName =
    | AclName of string // Allow-By-Group, Allow-By-User, etc...

    static member getText =
        function
        | AclName name -> name

type AclRefId = AclRefId of string
type NavId = NavId of string

type AclDisplay = {
    Reference: AclRefId
    DisplayName: string
}

type AclLookup<'t> = Map<AclName, Map<AclRefId, 't>>

type ResolvedAclLookup = AclLookup<AclDisplay>
