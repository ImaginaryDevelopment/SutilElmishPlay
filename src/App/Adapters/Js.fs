module Core

open BReusable

open Fable.Core
open Fable.Core.JsInterop

let log x = Browser.Dom.console.log (x)

// https://medium.com/@zaid.naom/f-interop-with-javascript-in-fable-the-complete-guide-ccc5b896a59f
[<Emit("JSON.stringify($0,null,\" \")")>]
let pretty<'t> (x: 't) =
    JS.JSON.stringify (x, Unchecked.defaultof<_>, " ") //JSON.stringify(data, null, "  ")

let serialize<'t> (x: 't) = JS.JSON.stringify (x) //JSON.stringify(data, null, "  ")

// this may have issues with DUs and other specialized types
let deserialize<'t> (x: string) =
    Option.ofValueString x
    |> Option.map JS.JSON.parse
    |> Option.bind Option.ofUnsafe
    |> Option.map (fun v -> v :?> 't)

[<Emit("Object.keys($0)")>]
let keys x : string[] = jsNative

// [<Emit("Object.assign($0)")>]
// let assign<'t> (target:obj, [<System.ParamArray>] items: obj): 't = jsNative

[<Emit("Object.assign({},$0)")>]
let clone<'t> (source: 't) : 't = jsNative

let cloneSet<'t> (source: 't) (propName: string) (value: obj) =
    let nextItem = clone source
    // formatter may break this line
    nextItem?(propName) <- value
    nextItem


[<Emit("delete $0[$1]")>]
let delete (target: obj) (name: string) = jsNative

[<Emit("debugger;")>]
let debugger () : unit = jsNative

let cloneDiagnose source =
    let v = clone source
    debugger ()
    v

let cloneExcept (source: obj, exclusions: string seq) =
    let v = clone source
    exclusions |> Seq.iter (fun name -> delete v name)
    // debugger()
    v

let inline toGlobal (name: string) (value: obj) : unit =
    printfn "Adding global %s" name
    // Browser.Dom.self?(name) <- value
    Browser.Dom.window?(name) <- value

// let y = {|testing="Hello";v="world"|}
// log("Cloned:")
// log(clone y)
// log(cloneExcept(y, ["v"]))
// Fable.Core.JS.console.log("cloned:", clone y)

[<Emit("$0[$1]")>]
let getValue (x: obj) (name: string) = jsNative

let mutable debug = false

let tryParse<'t> title (x: string) : Result<'t, exn> =
    try
        let parsed = JS.JSON.parse (x)

        if debug then
            printfn $"%s{title}: Parsed"

        let cast = parsed :?> 't

        if debug then
            printfn $"%s{title}: Cast"

        Ok cast
    with ex ->
        Error ex

module Handlers =
    let getValue (e: Browser.Types.Event) : string = e.target?value

()

module LocalStorage =
    module internal Impl =
        let localStorage = Browser.Dom.self.localStorage

    open Impl

    type Internal =
        // let private localStorage = Browser.Dom.self.localStorage
        // let private json = Fable.Core.JS.JSON
        static member inline TryGet<'t when 't: equality>(key) : 't option =
            localStorage.getItem key
            |> Option.ofValueString
            |> Option.bind (fun x ->
                // printfn "Found %s -> %s" key s
                let result: 't option =
                    // Thoth.Json.Decode.Auto.fromString s
                    // Resolver.Deserialize(x)
                    deserialize x
                // json.parse(s)
                // |> unbox
                result)

        static member inline TrySave(key: string, valueOpt: 't option) : Result<unit, string> =
            printfn "trying to save"

            try
                // let pojo = Fable.Core.JsInterop.toPlainJsObj value
                let serial =
                    match valueOpt with
                    | Some(value: 't) ->
                        let stringy = serialize value
                        stringy
                    | None -> null

                printfn "Saving to key %s" key

                localStorage.setItem (key, serial)
                // printfn "Saved -> %s" serial
                Ok()
            with ex ->
                toGlobal "self" Browser.Dom.self
                Error(ex.Message)

    type IAccessor<'t> = //
        abstract member GetValue: unit -> 't option
        abstract member TryGetValue: unit -> 't option
        abstract member TrySetValue: 't option -> Result<unit, string>

    // assumes we never want to clear a key entirely
    // assumes the serializer/deserializer works well enough
    type StorageAccess<'t when 't: equality>(name) =
        let get () = Internal.TryGet<'t> name

        do
            toGlobal $"storageAccess_{name}" get
            ()

        static member CreateStorage(name) = StorageAccess(name)

        member inline this.TryGet() =
            try
                this.Get()
            with ex ->
                eprintfn "Failed to fetch from storage '%s'" name
                log ex
                None

        member inline _.Get() = get ()
        member inline _.Save(x: 't option) = Internal.TrySave(name, x)

        interface IAccessor<'t> with
            override this.GetValue() = this.Get()
            override this.TryGetValue() = this.TryGet()
            override this.TrySetValue valueOpt = this.Save valueOpt

    type TypeMapper<'t1, 't2> = {
        Setter: 't1 -> 't2
        Getter: 't2 -> 't1
    }

    type StorageAccessor<'t, 'tRest when 'tRest: equality>(name: string, fMaps: TypeMapper<'t, 'tRest>) =
        let sa: StorageAccess<'tRest> = StorageAccess.CreateStorage name

        interface IAccessor<'t> with
            override _.GetValue() = sa.Get() |> Option.map fMaps.Getter
            override _.TryGetValue() = sa.TryGet() |> Option.map fMaps.Getter

            override _.TrySetValue valueOpt =
                valueOpt |> Option.map fMaps.Setter |> sa.Save
