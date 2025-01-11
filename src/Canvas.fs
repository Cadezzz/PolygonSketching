module Canvas 

open Fable.Core
open Feliz
open Elmish

type Coord = { x : float; y : float }

type PolyLine = list<Coord>

type Model = {
    finishedPolygons: list<PolyLine>
    currentPolygon: Option<PolyLine>
    mousePos: Option<Coord>
    history: list<Model> 
    future: list<Model> 
}

type Msg =
    | AddPoint of Coord
    | SetCursorPos of Option<Coord>
    | FinishPolygon
    | Undo
    | Redo

let init () =
    { 
        finishedPolygons = []
        currentPolygon = None
        mousePos = None
        history = []
        future = []
    }, Cmd.none

let saveHistory (model: Model) =
    { model with history = model :: model.history; future = [] }

let update (msg: Msg) (model: Model) =
    match msg with
    | AddPoint coord ->
        let updatedModel =
            match model.currentPolygon with
            | None -> { model with currentPolygon = Some [coord] }
            | Some polyline -> { model with currentPolygon = Some (coord :: polyline) }
        saveHistory updatedModel, Cmd.none

    | SetCursorPos pos -> 
        { model with mousePos = pos }, Cmd.none

    | FinishPolygon ->
        match model.currentPolygon with
        | None -> model, Cmd.none
        | Some polyline ->
            let updatedModel = 
                saveHistory {
                    model with
                        finishedPolygons = polyline :: model.finishedPolygons
                        currentPolygon = None
                }
            updatedModel, Cmd.none

    | Undo ->
        match model.history with
        | [] -> model, Cmd.none
        | prev :: rest -> { prev with history = rest; future = model :: model.future }, Cmd.none

    | Redo ->
        match model.future with
        | [] -> model, Cmd.none
        | next :: rest -> { next with history = model :: model.history; future = rest }, Cmd.none

[<Emit("getSvgCoordinates($0)")>]
let getSvgCoordinates o: Coord = jsNative

let viewPolygon (color: string) (polyline: PolyLine): list<ReactElement> =
    match polyline with
    | [] -> []
    | points ->
        let pointsString = 
            points 
            |> List.rev
            |> List.map (fun point -> $"{point.x},{point.y}")
            |> String.concat " "

        [
            Svg.polygon [
                svg.points pointsString
                svg.fill "none"
                svg.stroke color
                svg.strokeWidth 2.0
            ]
        ]

let viewLine (color: string) (start: Coord) (end_: Coord): ReactElement =
    Svg.line [
        svg.x1 start.x; svg.y1 start.y
        svg.x2 end_.x; svg.y2 end_.y
        svg.stroke color; svg.strokeWidth 1.0
    ]

let render (model: Model) (dispatch: Msg -> unit) =
    let border =
        Svg.rect [
            svg.x 0; svg.y 0
            svg.width 500; svg.height 500
            svg.stroke "black"; svg.strokeWidth 2.0; svg.fill "none"
        ]

    let finishedPolygons =
        model.finishedPolygons |> List.collect (viewPolygon "green")

    let currentPolygon =
        match model.currentPolygon with
        | None -> []
        | Some polyline ->
            let mainLine = 
                match model.mousePos with
                | None -> viewPolygon "red" polyline
                | Some pos -> viewPolygon "red" (pos :: polyline)

            let dynamicMouseLine =
                match model.mousePos with
                | None -> []
                | Some pos -> 
                    match List.tryLast polyline with
                    | Some last -> [ viewLine "red" last pos ]
                    | None -> []

            mainLine @ dynamicMouseLine

    let svgElements = List.concat [ finishedPolygons; currentPolygon ]

    Html.div [
        prop.children [
            Html.h1 "Polygon Drawing"
            Html.button [
                prop.style [ style.margin 20 ]
                prop.onClick (fun _ -> dispatch Undo)
                prop.text "Undo"
            ]
            Html.button [
                prop.style [ style.margin 20 ]
                prop.onClick (fun _ -> dispatch Redo)
                prop.text "Redo"
            ]
            Svg.svg [
                svg.width 500; svg.height 500
                svg.onMouseMove (fun ev ->
                    let pos = getSvgCoordinates ev
                    dispatch (SetCursorPos (Some pos))
                )
                svg.onClick (fun ev ->
                    if ev.detail = 1 then
                        dispatch (AddPoint (getSvgCoordinates ev))
                    elif ev.detail = 2 then
                        dispatch FinishPolygon
                )
                svg.children (border :: svgElements)
            ]
        ]
    ]
