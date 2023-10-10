open System

module Option =
    let ofFileExists path =
        if String.IsNullOrEmpty path then failwith $"bad path"
        if IO.File.Exists path then
            Some path
        else
            let fp = IO.Path.GetFullPath path
            printfn $"Path '{path}' ({fp})"
            None

module Map =
    // which one wins is not tested
    let combine maps =
        (Map.empty, maps)
        ||> List.fold(fun m m2 ->
            m |> Map.fold (fun acc key value -> Map.add key value acc) m2
        )

let (|ValueString|NonValueString|) value =
    if String.IsNullOrWhiteSpace value then
        NonValueString ()
    else
        ValueString value

let (|StrEqualsI|_|) delimiter (value:string) =
    if String.IsNullOrEmpty delimiter then
        failwith $"Bad delimiter"
    if String.Equals(value,delimiter, StringComparison.InvariantCultureIgnoreCase) then
        Some ()
    else None

let tryFindEnv =
    let vars =
        System.Environment.GetEnvironmentVariables()
        |> Seq.cast<System.Collections.DictionaryEntry>
        |> Seq.map (fun d -> d.Key :?> string, d.Value :?> string)
        |> Map.ofSeq

    fun name ->
        vars
        |> Map.tryFindKey(fun k _ ->
            System.String.Equals(k,name, StringComparison.InvariantCultureIgnoreCase)
        )
        |> Option.map(fun k -> vars[k])
// rundll32 sysdm.cpl,EditEnvironmentVariables 
module DotEnv =
    let loadEnv path : Map<string,string> =
        IO.File.ReadAllLines(path=path)
        |> Array.choose(fun value ->
            value.Split('=')
            |> List.ofArray
            |> function
                | [] -> eprintfn "Value not understood: '%s'" value; None
                | _ :: [] -> eprintfn "Value not understood: '%s'" value; None
                | h::tl -> Some (h, tl |> String.concat "=")
        )
        |> Map.ofArray

    let buildEnvPath env =
        if String.IsNullOrEmpty env then
            ".env"
        else
            $".env.{env}"
    let tryLoadEnvFile env =
        buildEnvPath env
        |> Option.ofFileExists
        |> Option.map loadEnv

    let envMap =
        match tryFindEnv "NODE_ENV" with
        | Some (StrEqualsI "production") ->
            Some "prod"
        | Some (ValueString x) ->
            Some x
        | Some _ -> None
        | None -> None
        |> Option.bind tryLoadEnvFile
    let loadEnvs() =
        [
            Some <| Map.ofList [("NODE_ENV", "local")]
            tryLoadEnvFile null
            envMap
        ]
        |> List.choose id
        |> Map.combine

module Process =
    open System.Diagnostics
    let createInfo fileName args useShell =
        let args = String.concat " " args
        let psi = ProcessStartInfo(fileName, args, UseShellExecute=useShell)
        psi

    let addEnv name value (psi: ProcessStartInfo) =
        psi.EnvironmentVariables.Add(name,value)

// (Process.createInfo "npm" ["start"] true, DotEnv.loadEnvs())
// ||> Map.fold(fun psi k v ->
//     Process.addEnv k v psi
//     psi
// )

DotEnv.loadEnvs()
|> Map.iter(fun k v ->
    printfn "Setting '%s'='%s'" k v
    Environment.SetEnvironmentVariable(k,v,EnvironmentVariableTarget.Process)
    ()
)
