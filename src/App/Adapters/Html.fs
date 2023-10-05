module App.Adapters.Html

open Sutil

let data_ (name:string) value = prop.custom($"data-{name}",value)