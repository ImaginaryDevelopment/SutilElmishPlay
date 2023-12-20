open System
open System.IO

// TODO: would be nice if this could produce something for gh-pages

#load "src\\App\\BReusable.fs"

open BReusable



let outputFolder = "public"

//
let publishRoot =
    Path.Combine("..", Environment.ExpandEnvironmentVariables(@"%sutil_target%"), "public") // cspell:disable-line

printfn "PublishRoot: %s" publishRoot
let publishTargetFolder = Path.Combine(publishRoot, "admin")
let bundlePath = Path.GetFullPath(Path.Combine(outputFolder, "bundle.js"))
let buildInfoPath = Path.Combine(outputFolder, "build_info.txt") |> Path.GetFullPath

let endsWith (delimiter: char) (value: string) = value.EndsWith delimiter

let failNullOrEmpty title v =
    if String.IsNullOrEmpty v then
        failwith title

let replace (delimiter: string) (replacement: string) (value: string) =
    failNullOrEmpty "delimiter" delimiter

    match value with
    | null -> failwith "Null replace target"
    | "" -> value
    | _ -> value.Replace(delimiter, replacement)

let existsOrFail msg path =
    if String.IsNullOrWhiteSpace path then
        failwith "Bad path"

    if IO.File.Exists path || IO.Directory.Exists path then
        ()
    else
        failwith msg

let (|ExistingDir|ExistingFile|NonExistent|) path =
    if String.IsNullOrWhiteSpace path then
        failwith "Bad Path"

    if Directory.Exists path then ExistingDir path
    elif File.Exists path then ExistingFile path
    else NonExistent

module Directory =
    let makeExist path =
        if Directory.Exists path then
            ()
        else
            Directory.CreateDirectory path |> ignore

    // https://stackoverflow.com/questions/58744/copy-the-entire-contents-of-a-directory-in-c-sharp
    let copy source target =
        let source = Path.GetFullPath source + string Path.DirectorySeparatorChar
        let target = Path.GetFullPath target + string Path.DirectorySeparatorChar

        if source |> endsWith Path.DirectorySeparatorChar |> not then
            failwith $"Bad copy request: source: '%s{source}'"

        if target |> endsWith Path.DirectorySeparatorChar |> not then
            failwith $"Bad copy request: target: '%s{target}'"

        Directory.GetDirectories(source, "*", SearchOption.AllDirectories)
        |> Seq.iter (fun sp ->

            let targetSp = sp |> replace source target

            match targetSp with
            | ExistingDir _ -> ()
            | ExistingFile _ -> failwith "File exists at directory target"
            | NonExistent -> Directory.CreateDirectory targetSp |> ignore)

        Directory.GetFiles(source, "*", SearchOption.AllDirectories)
        |> Seq.iter (fun sp ->
            let targetSp = sp |> replace source target
            File.Copy(sp, targetSp, overwrite = true))

if File.Exists bundlePath then
    File.Delete "bundle.js"

if File.Exists buildInfoPath then
    File.Delete buildInfoPath

module Process =
    let runProcess cmd args =
        let psi =
            Diagnostics.ProcessStartInfo(cmd, arguments = args, UseShellExecute = true)

        use p = Diagnostics.Process.Start psi
        p.WaitForExit()
        if p.ExitCode <> 0 then Error p.ExitCode else Ok()

    let runCaptured cmd args =
        let psi =
            Diagnostics.ProcessStartInfo(
                cmd,
                arguments = args,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            )

        use p = Diagnostics.Process.Start psi
        let data = ResizeArray()

        use _ =
            p.OutputDataReceived.Subscribe(fun d ->
                d.Data |> Option.ofValueString |> Option.map Ok |> Option.iter data.Add)

        use _ =
            p.ErrorDataReceived.Subscribe(fun e ->
                e.Data |> Option.ofValueString |> Option.map Error |> Option.iter data.Add)

        p.Start() |> ignore
        p.BeginErrorReadLine()
        p.BeginOutputReadLine()
        p.WaitForExit()
        let data = data |> Seq.toList

        if p.ExitCode <> 0 then
            Error p.ExitCode, data
        else
            Ok(), data

module BuildInfo =
    let getSingleLineOutput =
        function
        | Ok(), Ok h :: [] -> h
        | Ok(), data -> failwith $"Expected single line line output: %A{data}"
        | Error ec, data -> failwith $"Exited: %i{ec} -> %A{data}"

    type HashInputType =
        | Bytes of byte[]
        | ByStream of Stream
        | FilePath of string

    let hash hi =
        use sha = System.Security.Cryptography.MD5.Create()

        match hi with
        | Bytes b -> sha.ComputeHash b
        | ByStream stream -> sha.ComputeHash stream
        | FilePath fp ->
            use fs = new FileStream(fp, FileMode.Open, FileAccess.Read, FileShare.Read)
            sha.ComputeHash fs
        |> Convert.ToBase64String

    let gatherBuildInfo () =
        let sb = System.Text.StringBuilder()

        let gitLogInfo =
            Process.runCaptured "git" "log --oneline -n1" // cspell:ignore oneline
            |> getSingleLineOutput

        let gitBranchInfo =
            Process.runCaptured "git" "branch --show-current" |> getSingleLineOutput

        let hashes =
            [
                "CHANGELOG.md"
                "README.md"
                "package.json"
                "package-lock.json"
                "ReleaseHistory.md"
                "public\\index.html"
                "publish.fsx"
            ]
            |> List.map (fun n ->
                let h = FilePath n |> hash
                $"\t%s{n}:%A{h}")
            |> String.concat Environment.NewLine

        [
            "// cspell: disable"
            DateTime.Now.ToString("yyyy.MM.dd hh:mm:ss zzz")
            "Git:"
            gitBranchInfo
            gitLogInfo
            "Hashes:"
            hashes
        ]
        |> Seq.iter (sb.AppendLine >> ignore)

        string sb

let buildInfo = BuildInfo.gatherBuildInfo ()
printfn "BuildInfo?: %s" buildInfo
File.WriteAllText(buildInfoPath, buildInfo)

failwith "stop now"
Process.runProcess "npm" "run build"

match Directory.GetFiles(outputFolder, "*.LICENSE.txt") |> List.ofArray with
| [] -> eprintfn "no license file found"
| items -> items |> Seq.iter File.Delete

existsOrFail "PublishRoot" publishRoot

Directory.makeExist publishTargetFolder


Directory.copy outputFolder publishTargetFolder
