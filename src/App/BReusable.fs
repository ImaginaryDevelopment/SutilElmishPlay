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

    let equalsI (value: string) (x: string) =
        isValueString x
        && isValueString value
        && x.Equals(value, System.StringComparison.InvariantCultureIgnoreCase)

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

    let replace (delimiter: string) (replacement: string) (value: string) =
        if not <| isValueString delimiter then
            failwithf "no delimiter passed"

        match value with
        | null -> null
        | _ -> value.Replace(delimiter, replacement)

    // skip allows debug overrides
    let truncateDisplay skip limit value =
        if limit < 1 then
            failwith "Limit was less than 1"

        let len =
            value |> Option.ofObj |> Option.map (String.length) |> Option.defaultValue -1

        if not skip && len > limit then
            // what if the length isn't greater than 3?
            if len < 4 then
                value[0..limit]
            else
                value[0 .. limit - 3] + "..."
        else
            value


let (|ValueString|NonValueString|) value =
    if String.isValueString value then
        ValueString value
    else
        NonValueString()

module Option =
    let getOrFail msg =
        function
        | None -> failwith msg
        | Some v -> v

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

    let ofResult x =
        match x with
        | Ok v -> Some v, None
        | Error e -> None, Some e

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

    let ofValueList =
        function
        | [] -> None
        | x -> Some x

let (|EqualsI|_|) y = Option.ofBool (String.equalsIStr y)

let equalsIStr (item: 't) (x: string) =
    match x with
    | EqualsI(string (box item)) -> Some item
    | _ -> None

let startsWithI (delimiter: string) (value: string) =
    Option.ofValueString value
    |> function
        | Some value -> value.StartsWith(delimiter, System.StringComparison.CurrentCultureIgnoreCase)
        | None -> false

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
    let ofValue x = async { return x }

module Parse =
    let tryBool (x: string) =
        System.Boolean.TryParse x
        |> function
            | true, v -> Some v
            | false, _ when x = "on" -> Some true
            | false, _ -> None

    let tryInt (x: string) : int option =
        System.Int32.TryParse x
        |> function
            | true, v -> Some v
            | false, _ -> None

let (|LazyStill|LazyValue|) (l: Lazy<_>) =
    if l.IsValueCreated then LazyValue l.Value else LazyStill

module Lazy =
    let get (l: Lazy<_>) = l.Value

    let ifCreated (l: Lazy<_>) =
        if l.IsValueCreated then Some l.Value else None

module Array =
    // updateAt exists but gives exceptions and doesn't pass the oldItem to the update function

    let updateI i fUpdate items =
        if i < 0 then
            Error "Bad index"
        elif i >= Array.length items then
            Error "Index out of bounds"
        else
            items |> Array.mapi (fun x item -> if i <> x then item else fUpdate item) |> Ok

    let updateItem findIndexPredicate fUpdate items =
        items
        |> Array.tryFindIndex findIndexPredicate
        |> function
            | None -> Error "Could not find item"
            | Some x -> items |> Array.mapi (fun i item -> if i <> x then item else fUpdate item) |> Ok

module Set =
    let toggle value set =
        if set |> Set.contains value then
            set |> Set.remove value
        else
            set |> Set.add value

module Map =
    let upsert key value (m: Map<_, _ list>) =
        m
        |> Map.change key (fun others -> value :: (others |> Option.defaultValue List.empty) |> Some)
    // if m |> Map.containsKey key then
    //     m |> Map.add key (value :: m[key])
    // else
    //     m |> Map.add key [ value ]

    // does not remap any keys, choosing values only
    let mapChoose f m =
        (Map.empty, m)
        ||> Map.fold (fun m k v ->
            match f k v with
            | None -> m
            | Some fv -> m |> Map.add k fv)

    let choose f m =
        (Map.empty, m)
        ||> Map.fold (fun m k v ->
            match f k v with
            | None -> m
            | Some(fk, fv) -> m |> Map.add fk fv)

    let ofSeqGroups items =
        items
        |> Seq.groupBy fst
        |> Map.ofSeq
        |> Map.map (fun _ v -> v |> Seq.map snd |> List.ofSeq)

    let addChildren key values m =
        m
        |> Map.change key (function
            | Some existing -> Some(List.ofSeq values @ existing)
            | None -> Some(List.ofSeq values))

    let addNest rootKey subKey value (m: Map<_, Map<_, _>>) =
        m
        |> Map.tryFind rootKey
        |> function
            | None -> m |> Map.add rootKey ([ subKey, value ] |> Map.ofSeq)
            | Some subMap -> m |> Map.add rootKey (subMap |> Map.add subKey value)

    let tryGetValueMap m = if Map.isEmpty m then None else Some m

// let ofSeqPairs items =
//     (Map.empty, items)
//     ||> Seq.fold(fun m (key,value) ->
//         m |> upsert key value
//     )

// System.Collections.Generic.
type Observed<'t>(valueOpt: 't option) =
    let sync = obj ()
    let mutable value = valueOpt
    let observers = System.Collections.Generic.List<System.IObserver<'t>>()
    let remove observer () = observers.Remove observer |> ignore

    member _.SetValue v =
        value <- Some v
        observers |> Seq.iter (fun obs -> obs.OnNext v)

    member _.Value = value


    interface System.IObservable<'t> with
        member this.Subscribe(observer: System.IObserver<'t>) : System.IDisposable =
            observers.Add observer

            { new System.IDisposable with
                member this.Dispose() = lock sync <| remove observer
            }

// my own take that includes the ability to peek the current value
// based on https://stackoverflow.com/questions/54031423/how-do-i-create-a-subject-f-observable
type Subject<'t>(current: 't option) =
    let sync = obj ()
    let mutable current = current
    let mutable stopped = false
    let observers = ResizeArray<System.IObserver<'t>>()
    let iter f = observers |> Seq.iter f

    let onCompleted () =
        if not stopped then
            stopped <- true
            iter (fun observer -> observer.OnCompleted())

    let onError ex () =
        if not stopped then
            stopped <- true
            iter (fun observer -> observer.OnError ex)

    let next value () =
        if not stopped then
            current <- Some value
            iter (fun observer -> observer.OnNext(value))

    let remove observer () = observers.Remove observer |> ignore

    member _.Value = current
    member _.Next value = lock sync <| next value
    member _.Error ex = lock sync <| onError ex
    member _.Completed() = lock sync <| onCompleted

    interface System.IObserver<'t> with
        member x.OnCompleted() = x.Completed()
        member x.OnError ex = x.Error ex
        member x.OnNext value = x.Next value

    interface System.IObservable<'t> with
        member this.Subscribe(observer: System.IObserver<'t>) =
            observers.Add observer

            { new System.IDisposable with
                member this.Dispose() = lock sync <| remove observer
            }

// pass the current value, and an observable
type ObsSlip<'t>(value: 't, obs: System.IObservable<'t>) =
    let mutable value = value
    let d = obs.Subscribe(fun v -> value <- v)
    member _.Value = value

    interface System.IObservable<'t> with
        member this.Subscribe(observer: System.IObserver<'t>) = obs.Subscribe observer

    interface System.IDisposable with
        member _.Dispose() = d.Dispose()

module ObsSlip =
    let map f (x: ObsSlip<_>) =
        let value = f x.Value
        new ObsSlip<_>(value, x |> Observable.map f)
// type ObsOptSlip<'t>(value: 't option, obs: System.IObservable<'t>) =
//     let mutable value = value
//     let d = obs.Subscribe(fun v -> value <- Some v)
//     member _.TryGetValue() = value

//     interface System.IObservable<'t> with
//         member this.Subscribe(observer: System.IObserver<'t>) = obs.Subscribe observer

//     interface System.IDisposable with
//         member _.Dispose() = d.Dispose()

let (|ValueSeqOption|_|) (x: #seq<_> option) =
    match x with
    | None -> None
    | Some x when Seq.isEmpty x -> None
    | Some x -> Some x
