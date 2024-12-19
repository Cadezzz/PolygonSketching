module Canvas 

open Fable.Core
open Feliz
open Elmish

type Coord = { x : float; y : float }

type Circle =
    {
        pos: Coord
    }

type Model = { 
    circles : list<Circle>  
    past : Option<Model>
}

type Msg =
    | AddPoint of Coord
    | Undo

let init () =
    { 
        past = None
        circles = 
            [
                { pos = { x = 50.0; y = 50.0 } }
                { pos = { x = 150.0; y = 150.0 } }
                { pos = { x= 100.0; y = 100.0 } }
            ]
    }
    , Cmd.none


let update (msg : Msg) (currentModel : Model)  =
    match msg with
    | AddPoint p -> 
        { currentModel with 
            circles = { pos = p; } :: currentModel.circles 
            past = Some currentModel
        }, Cmd.none
    | Undo ->
        match currentModel.past with
        | None -> currentModel, Cmd.none
        | Some p -> p, Cmd.none


[<Emit("getSvgCoordinates($0)")>]
let getSvgCoordinates o: Coord = jsNative

let render (model : Model) (dispatch : Msg -> unit) =
     
    let points = 
        model.circles |> List.mapi (fun index point -> 
            Svg.circle [
                svg.cx point.pos.x
                svg.cy point.pos.y
                svg.r 3.0 
                svg.fill color.aliceBlue
                svg.stroke color.gray
                svg.strokeWidth 1
            ]
        )

    let border = 
        Svg.rect [
            svg.x1 0
            svg.x2 500
            svg.y1 0
            svg.y2 500
            svg.width 500
            svg.height 500
            svg.stroke("black") 
            svg.strokeWidth(2)
            svg.fill "none"
        ] 
 

    Html.div [
        prop.children [
            Html.h1 "Simplest CANVAS..."
            Html.button [
                prop.style [style.margin 20]; 
                prop.onClick (fun _ -> dispatch Undo)
                prop.children [Html.text "undo"]
            ]
            Html.br []
            Svg.svg [
                svg.width 500
                svg.height 500
                svg.onClick (fun mouseEvent -> 
                    //System.Diagnostics.Debugger.Break()
                    let pos = getSvgCoordinates mouseEvent
                    AddPoint pos |> dispatch)
                svg.children (border :: points)
            ]
        ]
    ]