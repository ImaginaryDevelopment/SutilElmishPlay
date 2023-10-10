module App.Adapters.Html

open Sutil

let data_ (name:string) value = prop.custom($"data-{name}",value)

let columns2 col1 col2 =
    Html.divc "columns" [
        Html.divc "column" col1
        Html.divc "column" col2
    ]

module Handlers =
    let onValueChange<'t> (dispatch:'t -> unit) f = 
        Sutil.CoreElements.on "change" (Core.Handlers.getValue >> f >> dispatch) List.empty