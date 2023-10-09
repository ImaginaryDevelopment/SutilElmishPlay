open System.Text
let targets =
    System.IO.Directory.GetFiles(@"node_modules\@mui\icons-material", "*.js")
    |> Array.filter(fun f -> not <| f.EndsWith "index.js")

printfn "Searching %i files" targets.Length

let indexOf delimiter (value:string) =
    if System.String.IsNullOrEmpty delimiter then
        failwith "Bad delimiter"
    match value.IndexOf delimiter with
    | i when i >= 0 -> Some i
    | _ -> None

let after delimiter value =
    match indexOf delimiter value with
    | Some i -> value[i+delimiter.Length .. ]
    | None -> failwithf "Value not found '%s'" delimiter

let before delimiter value =
    match indexOf delimiter value with
    | Some i -> value[0..i - 1]
    | None -> failwithf "Value not found '%s'" delimiter
let mutable skipped = 0
let valuesToAttempt =
    targets
    |> Seq.indexed
    |> Seq.choose(fun (i,f) ->
        //   d: "M7.5 4C5.57 4 4 5.57 4 7.5S5.57 11 7.5 11 11 9.43 11 7.5 9.43 4 7.5 4zm0 5C6.67 9 6 8.33 6 7.5S6.67 6 7.5 6 9 6.67 9 7.5 8.33 9 7.5 9zm9 4c-1.93 0-3.5 1.57-3.5 3.5s1.57 3.5 3.5 3.5 3.5-1.57 3.5-3.5-1.57-3.5-3.5-3.5zm0 5c-.83 0-1.5-.67-1.5-1.5s.67-1.5 1.5-1.5 1.5.67 1.5 1.5-.67 1.5-1.5 1.5zm2.79-13.29c.39.39.39 1.02 0 1.41L6.12 19.29c-.39.39-1.02.39-1.41 0s-.39-1.02 0-1.41L17.88 4.71c.39-.39 1.02-.39 1.41 0z"
        if i % 100 = 0 then
            printfn "Starting file %i" i
        let fileText = System.IO.File.ReadAllText f
        if fileText.Contains "\"path\"" then
            try
                let name =
                    fileText
                    |> after "d:"
                    |> after ","
                    |> after "'"
                    |> before "'"
                let d =
                    fileText
                    |> after "d:"
                    |> after "\""
                    |> before "\""
                Some (f, (name, d))
            with _ ->
                eprintfn "Failing on %s" f
                reraise()
        else
            skipped <- skipped - 1
            printfn "Skipping '%s'" f
            None
    )
    |> List.ofSeq

// whole process blew up if it was into a list of length 4841
valuesToAttempt
|> Map.ofSeq

|> fun m ->
    let targetPath = @"src\App\Adapters\Mui.generated.fs"
    let sb = StringBuilder()
    let appendLine (x:string) = sb.AppendLine(x) |> ignore
    // m |> Map.iter(fun f (k,v) ->
    //     appendLine($"// {f}")
    //     appendLine($"let {k}=@\"{v}\"")
    // )
    appendLine "module App.Adapters.Mui"
    appendLine ""
    // appendLine "let all = Map.ofSeq ["
    appendLine "let all = Map.ofArray [|"
    m |> Map.iter(fun f (k,v)->
        // appendLine($"    // {f}")
        appendLine $"    \"{k}\",\"{v}\""
    )
    appendLine "|]"
    System.IO.File.WriteAllText(targetPath,sb.ToString())