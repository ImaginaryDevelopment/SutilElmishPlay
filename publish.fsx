open System
open System.IO

let outputFolder = "public"

//
let publishRoot = "..\\oneview_web\\public" // cspell:disable-line
let publishTargetFolder = Path.Combine(publishRoot, "admin")
let bundlePath = Path.GetFullPath(Path.Combine(outputFolder, "bundle.js"))

let endsWith (delimiter:char) (value:string) =
    value.EndsWith delimiter
let failNullOrEmpty title v =
    if String.IsNullOrEmpty v then failwith title

let replace (delimiter:string) (replacement:string) (value:string) =
    failNullOrEmpty "delimiter" delimiter
    match value with
    | null -> failwith "Null replace target"
    | "" -> value
    | _ -> value.Replace(delimiter, replacement)

let existsOrFail msg path =
    if String.IsNullOrWhiteSpace path then failwith "Bad path"
    if IO.File.Exists path || IO.Directory.Exists path then ()
    else failwith msg
let (|ExistingDir|ExistingFile|NonExistent|) path =
    if String.IsNullOrWhiteSpace path then failwith "Bad Path"
    if Directory.Exists path then ExistingDir path
    elif File.Exists path then ExistingFile path
    else NonExistent

module Directory =
    let makeExist path =
        if Directory.Exists path then
            ()
        else Directory.CreateDirectory path |> ignore

    // https://stackoverflow.com/questions/58744/copy-the-entire-contents-of-a-directory-in-c-sharp
    let copy source target =
        let source = Path.GetFullPath source + string Path.DirectorySeparatorChar
        let target = Path.GetFullPath target + string Path.DirectorySeparatorChar

        if source |> endsWith Path.DirectorySeparatorChar |> not then
            failwith $"Bad copy request: source: '%s{source}'"
        if target |> endsWith Path.DirectorySeparatorChar |> not then
            failwith $"Bad copy request: target: '%s{target}'"

        Directory.GetDirectories(source, "*", SearchOption.AllDirectories)
        |> Seq.iter(fun sp ->

            let targetSp = sp |> replace source target
            match targetSp with
            | ExistingDir _ -> ()
            | ExistingFile _ -> failwith "File exists at directory target"
            | NonExistent -> Directory.CreateDirectory targetSp |> ignore
        )
        Directory.GetFiles(source, "*", SearchOption.AllDirectories)
        |> Seq.iter(fun sp ->
            let targetSp = sp |> replace source target
            File.Copy(sp, targetSp, overwrite=true)
        )

if File.Exists bundlePath then
    File.Delete "bundle.js"

let runProcess cmd args =
    let psi = Diagnostics.ProcessStartInfo(cmd, arguments=args, UseShellExecute = true)
    use p = Diagnostics.Process.Start psi
    p.WaitForExit()
    if p.ExitCode <> 0 then
        Error p.ExitCode
    else Ok ()

runProcess "npm" "run build"

match Directory.GetFiles(outputFolder, "*.LICENSE.txt") |> List.ofArray with
| [] -> eprintfn "no license file found"
| items ->
    items |> Seq.iter File.Delete

existsOrFail "PublishRoot" publishRoot

Directory.makeExist publishTargetFolder

Directory.copy outputFolder publishTargetFolder

