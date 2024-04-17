module App.Adapters.Demo

open App.Schema
open App.Adapters.Api.Schema

let aclTypes: AclType[] = [|
    {
        Name = AclName "Allow-Academic"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-AllStaff"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Any"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Assistant-Principal"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-By-Grade-Level"
        MultiValue = true
        AclParamType =
            AclParameterType.Selectable(
                [|
                    "01"
                    "02"
                    "03"
                    "04"
                    "05"
                    "06"
                    "07"
                    "08"
                    "09"
                    "10"
                    "11"
                    "12"
                    "KG"
                    "PK"
                |]
            )
    }
    {
        Name = AclName "Allow-By-Group"
        MultiValue = true
        AclParamType = AclParameterType.Reference true
    }
    {
        Name = AclName "Allow-By-User"
        MultiValue = true
        AclParamType = AclParameterType.Reference true
    }
    {
        Name = AclName "Allow-DAT-User"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-None"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Outreach-User"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Parent"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Parent-By-Grade-Level"
        MultiValue = true
        AclParamType =
            AclParameterType.Selectable(
                [|
                    "01"
                    "02"
                    "03"
                    "04"
                    "05"
                    "06"
                    "07"
                    "08"
                    "09"
                    "10"
                    "11"
                    "12"
                    "KG"
                    "PK"
                |]
            )
    }
    {
        Name = AclName "Allow-PGA-AnyRole"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Principal"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Public"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Regional-Superintendent"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-School-Admin"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-School-Counselor"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-School-Personnel"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Student"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Superintendent"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
    {
        Name = AclName "Allow-Teacher"
        MultiValue = false
        AclParamType = AclParameterType.None
    }
|]
