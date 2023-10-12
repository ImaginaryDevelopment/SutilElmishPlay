module App.Components.NavEditor

open BReusable
open Core

open Fable.Core.JsInterop

open Sutil
open Sutil.CoreElements

open App.Adapters
open App.Adapters.Bulma
open App.Adapters.Html
open App.Adapters.Api

open App.Components.Gen
open App.Components.Gen.Icons

type EditorTabs =
    | IconTab
    | AclTab

type Model = {
    Tab: EditorTabs
    Item: NavRootResponse
}
type ParentMsg =
    | Cancel
    | Save of NavRootResponse

type EditorMsgType =
    | EditProp of prop:string * nextValue :string
    | EditAcl of AclEditor.AclParentMsg
    | TabChange of EditorTabs
    | IconMsg of IconEditor.IconEditorMsg

module Renderers =
    let renderIconEditor (propName, propObs) (value: string) (dispatch: Dispatch<EditorMsgType>) =
        let dispatch2 =
            function
            | App.Components.IconEditor.IconEditorMsg.NameChange(propName,value) ->
                dispatch (EditProp(propName,value))
        App.Components.IconEditor.renderIconEditor (propName, propObs) value dispatch2

    let renderEditorFrame (value:NavRootResponse) core dispatch =
        Html.divc "panel" [
            // path
            Html.pc "panel-heading" [
                if value.Type = "Folder" then
                    tryIcon (App.Init.IconSearchType.MuiIcon "FolderOpen")
                Html.span [
                    text $"{value.Name}: {value.Path}"
                ]
            ]
            Html.divc "panel-block" core
                // tabs [
                //     {
                //         Name= "Icon"
                //         TabClickMsg= Msg.TabChange RootTabs.RootEditor
                //         IsActive= rt = RootTabs.RootEditor
                //         Render=
                //             fun () -> renderIconEditor ("Icon", obs |> Observable.choose id |> Observable.map (fun v -> v.Icon) ) value.Icon dispatch // viewNavRoot (store,dispatch)
                //     }
                //     {
                //         Name= "Acls"
                //         TabClickMsg= Msg.TabChange RootTabs.RootEditor
                //         IsActive= rt = RootTabs.RootEditor
                //         Render = fun () ->
                //             renderAclsEditor value.Acls aclTypes (fun msg -> msg |> EditAcl |> Msg.EditorMsg |> dispatch)
                //     }
                // ] dispatch
                // // formField [ text "hello"] []
                // Html.divc "tile" [
                //     formField [text "Icon"] [
                //         Html.divc "box" [
                //             renderIconEditor ("Icon", obs |> Observable.choose id |> Observable.map (fun v -> v.Icon) ) value.Icon dispatch
                //         ]
                //     ]
                //     formField [ text "Acls"] [
                //         Html.divc "box" [
                //             renderAclsEditor value.Acls aclTypes (fun msg -> msg |> EditAcl |> Msg.EditorMsg |> dispatch)
                //         ]
                //     ]
                // ]
            Html.divc "panel-block" [
                Html.divc "left-left" [
                    Html.buttonc "button" [
                        text "Cancel"
                        onClick (fun _ -> Cancel |> dispatch) []
                    ]
                ]
            ]
            Html.pre [
                text (Core.pretty value)
            ]
        ]

let init item =
    {
        Tab=IconTab
        Item= item
    }

let update msg model =
    match msg with
    | TabChange t -> {model with Tab= t}
    | EditProp(name,value) ->
        let nextItem = clone model.Item
        (?) nextItem name <- value
        {model with Item= nextItem}
    | EditAcl aclMsg ->
        model  //of AclEditor.AclParentMsg

// renames will go a different route, no path editing
let renderEditor aclTypes (value:NavRootResponse, obs: System.IObservable<NavRootResponse option>) (dispatchParent: Dispatch<ParentMsg>) = 
    let store, dispatch = value |> Store.makeElmishSimple init update ignore

    let oldCore =
        Html.divc "tile" [
            formField [text "Icon"] [
                Html.divc "box" [
                    Renderers.renderIconEditor ("Icon", obs |> Observable.choose id |> Observable.map (fun v -> v.Icon) ) value.Icon dispatch
                ]
            ]
            formField [ text "Acls"] [
                Html.divc "box" [
                    AclEditor.renderAclsEditor value.Acls aclTypes (fun msg -> msg |> EditAcl |> dispatch)
                ]
            ]
        ]

    let core =
        tabs [
            {
                Name= "Icon"
                TabClickMsg= TabChange IconTab
                IsActive= store.Value.Tab = EditorTabs.IconTab
                Render=
                    fun () -> IconEditor.renderIconEditor ("Icon", obs |> Observable.choose id |> Observable.map (fun v -> v.Icon) ) value.Icon (IconMsg >> dispatch) // viewNavRoot (store,dispatch)
            }
            {
                Name= "Acls"
                TabClickMsg= TabChange AclTab
                IsActive= store.Value.Tab = AclTab
                Render = fun () ->
                    AclEditor.renderAclsEditor value.Acls aclTypes (fun msg -> msg |> EditAcl |> dispatch)
            }
        ] dispatch

    Renderers.renderEditorFrame value [
        core
        oldCore
    ] dispatchParent