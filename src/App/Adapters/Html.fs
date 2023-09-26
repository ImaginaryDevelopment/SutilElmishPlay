module App.Adapters.Html

open Sutil

let data_ name value = prop.custom($"data-{name}",value)