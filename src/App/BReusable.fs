module BReusable



let flip f x y = f y x

[<RequireQualifiedAccess>]
module String =
    let isValueString =
        function
        | null
        | "" -> false
        | x when System.String.IsNullOrWhiteSpace x -> false
        | _ -> true

    let equalsIStr (item: 't) (x: string) =
        isValueString x
        && x.Equals(string (box item), System.StringComparison.InvariantCultureIgnoreCase)

    let toLower =
        function
        | v when isValueString v -> v.ToLowerInvariant()
        | v -> v

    let toCharArray (x: string) = x.ToCharArray()

    let trim =
        function
        | null
        | "" -> ""
        | x -> x.Trim()

    let trim1 (delimiter: string list) =
        function
        | null
        | "" -> ""
        | x -> x.Trim(delimiter |> Array.ofList |> Array.collect toCharArray)

    let tryAfter delimiter (x: string) =
        if not <| isValueString delimiter then
            failwithf "no delimiter passed"

        match x.IndexOf delimiter with
        | x when x < 0 -> None
        | i -> Some x.[i + delimiter.Length ..]

    let tryBefore delimiter (x: string) =
        if not <| isValueString delimiter then
            failwithf "no delimiter passed"

        match x.IndexOf delimiter with
        | x when x < 0 -> None
        | i -> Some x.[0 .. i - 1]

let (|ValueString|NonValueString|) value =
    if String.isValueString value then
        ValueString value
    else
        NonValueString()

module Option =
    let ofValueString =
        function
        | ValueString v -> Some v
        | _ -> None

    let ofUnsafe (x: 't) =
        if System.Object.ReferenceEquals(null, x) then
            None
        else
            Some x

    let ofBool f x = if f x then Some() else None

    let ofOk x =
        match x with
        | Ok v -> Some v
        | _ -> None

    // f could also be an option
    let mapMaybe fOpt =
        function
        | None -> None
        | Some value ->
            match fOpt with
            | None -> Some value
            | Some f -> f value |> Some

    // f could also be an option
    let bindMaybe fOpt =
        function
        | None -> None
        | Some value ->
            match fOpt with
            | None -> Some value
            | Some f -> f value

let (|EqualsI|_|) y = Option.ofBool (String.equalsIStr y)

let equalsIStr (item: 't) (x: string) =
    match x with
    | EqualsI(string (box item)) -> Some item
    | _ -> None

let (|EqualsIStr|_|) (x: 't) = equalsIStr x

let (|After|_|) delimiter x = String.tryAfter delimiter x

let (|Before|_|) delimiter x = String.tryBefore delimiter x

module Result =
    let ofChoice =
        function
        | Choice1Of2 x -> Ok x
        | Choice2Of2 e -> Error e

module Tuple2 =
    let replicate x = (x, x)
    let map f (a, b) = f a, f b
    let mapSnd f (x, y) = x, f y
    let mapFst f (x, y) = f x, y
    let rotate (a, b) = b, a
    let combine f (a, b) = f a b
    let fromCurry x y = (x, y)

module Async =
    let map f x =
        async {
            let! value = x
            return f value
        }

    let catch x = x |> Async.Catch |> map Result.ofChoice
    let ofResult x = async { return x }
