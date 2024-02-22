module App.Adapters.Api.Mapped

open BReusable

open App.Adapters.Schema
open App.Adapters.Api.Schema
open App.Adapters.Api.Schema.Raw
open App.Adapters.Api.Shared
open App.Adapters.Api.Raw

type FieldName = string
// determine is creation based on empty id
type NavValidation = Result<ValidNavItem, Map<FieldName option, string list>>

and ValidNavItem = {
    ValidNavItem: NavItem
} with

    static member ValidateNavItem(({ Id = NavId itemId } as item): NavItem) : NavValidation =

        let isCreating = not <| String.isValueString itemId
        let isNested = String.isValueString item.Parent && item.Parent <> "/Root"

        let eMap =
            let e =
                [
                    "Creation distinction failed", Some(nameof item.Url), isCreating && String.isValueString itemId
                    "Cannot nest folders", Some(nameof item.Parent), item.Type = NavItemType.Folder && isNested
                    "Link: Empty Url",
                    Some(nameof item.Url),
                    item.Type = NavItemType.Link && not <| String.isValueString item.Url
                    "Name too short",
                    Some(nameof item.Name),
                    not <| String.isValueString item.Name || item.Name.Length < 3
                ]
                |> List.choose (fun (e, f, v) -> if v then Some(f, e) else None)

            (Map.empty, e) ||> List.fold (fun m (f, e) -> m |> Map.upsert f e)

        if Map.isEmpty eMap then
            Ok { ValidNavItem = item }
        else
            Error eMap

type NavPathResponse = { Path: string; Items: NavItem[] }

let getAclTypes token () =
    Raw.ApiAcls.getAclTypes token ()
    |> Async.map (
        Result.map (
            Array.map (fun apiAcl -> {
                Name = apiAcl.AclName
                MultiValue = apiAcl.MultiValue |> Option.defaultValue false
                AclParamType = apiAcl.AclParameterType
            })
        )
    )

module NavItemAdapters =

    // make sure that the path starts with "/root/"
    let ensureStartsWithRoot path =
        if path |> startsWithI "root/" then $"/{path}"
        elif path |> startsWithI "/root/" then path
        elif path |> startsWithI "/" then $"/root{path}"
        else $"/root/{path}"

    let ofApiNavItem (x: ApiNavItem) : NavItem =

        let mapped: NavItem = {
            Id = NavId x.Id
            Path = x.Path
            Parent = x.Parent
            Type =
                NavItemType.TryParse x.Type
                |> Option.getOrFail (sprintf "Could not parse '%s'" x.Type)
            Name = x.Name
            Description = x.Description
            Icon = x.Icon
            Weight = x.Weight
            Enabled = x.Enabled |> Option.defaultValue false
            Url = x.Url
            HasUrlKey = x.HasUrlKey
            AclRefs =
                x.Acls
                |> Seq.map (fun acl -> acl.AclName, acl.Parameters |> Set.ofArray)
                |> Map.ofSeq
        }

        mapped


    let toApiNavItem ({ Id = NavId navId } as item: NavItem) : ApiNavItem =
        let acls =
            item.AclRefs
            |> Map.toSeq
            |> Seq.map (fun (AclName aclName, pSet) -> {
                Name = aclName
                Parameters = pSet |> Set.toArray
            })
            |> Array.ofSeq

        {
            Acls = acls
            Description = item.Description
            Id = navId
            Path = item.Path |> ensureStartsWithRoot
            Parent = item.Parent
            Type = string item.Type
            Name = item.Name
            Icon = item.Icon
            Weight = item.Weight
            Enabled = item.Enabled |> Some
            Url = item.Url
            HasUrlKey = item.HasUrlKey
        }

    let toNavPathResponse (x: ApiNavInternals.ApiNavPathResponse) =
        let result: NavPathResponse = {
            Path = x.Path
            Items = x.Items |> Array.map ofApiNavItem
        }

        result


module NavItems =
    let getNavRoot token () =
        ApiNavInternals.getNavRoot token ()
        |> Async.map (Result.map (Array.map NavItemAdapters.ofApiNavItem))

    let getNavPath token path =
        let path =
            let starts = path |> startsWithI "/root"
            printfn "Starts? %A" starts

            if starts |> not then
                if path.StartsWith "/" then
                    $"/root{path}"
                else
                    $"/root/{path}"
            else
                path

        printfn "Fetching NavPath: %s" path

        ApiNavInternals.getNavPath token path
        |> Async.map (Result.map NavItemAdapters.toNavPathResponse)

    let create token (vni: ValidNavItem) =
        NavItemAdapters.toApiNavItem vni.ValidNavItem
        |> ApiNavInternals.create token
        |> Async.map (Result.map (NavItemAdapters.ofApiNavItem))

    let createV token (cni: NavItem) =
        ValidNavItem.ValidateNavItem cni |> Result.map (create token)

    let save token (item: NavItem) =

        let sItem = Core.serialize item

        using (Core.logGroup (Some "SaveItem"))
        <| fun _ ->
            Core.log sItem
            Core.log item

        let apiNavItem = NavItemAdapters.toApiNavItem item

        ApiNavInternals.save token apiNavItem
        |> Async.map (Result.map NavItemAdapters.ofApiNavItem)

    let delete token navId =
        Raw.deleteItem token navId
        |> Async.map (Result.map NavItemAdapters.ofApiNavItem)
