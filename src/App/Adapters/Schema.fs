module App.Adapters.Schema

type AclName = AclName of string
type AclRefId = AclRefId of string

type AclDisplay = {
    Reference: string // guid
    DisplayName: string
}

type ResolvedAclLookup = Map<AclName, Map<AclRefId, AclDisplay>>
