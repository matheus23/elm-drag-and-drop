module DragAndDrop.Divider exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as Attr


type Orientation
    = Horizontal
    | Vertical


view : Orientation -> Float -> List (Html.Attribute msg) -> (Float -> Html msg) -> Html msg
view orientation size attributes image =
    wrapper orientation <| overlapper orientation size attributes (image size)


match : { horiz : a, vert : a } -> Orientation -> a
match cases ori =
    case ori of
        Horizontal ->
            cases.horiz

        Vertical ->
            cases.vert


overlapper : Orientation -> Float -> List (Html.Attribute msg) -> Html msg -> Html msg
overlapper o size attributes elem =
    Html.div
        ([ Html.style
            [ ( "position", "absolute" )
            , ( match { horiz = "width", vert = "height" } o, "inherit" )
            , ( match { horiz = "height", vert = "width" } o, toString size ++ "px" )
            , ( match { horiz = "top", vert = "left" } o, "-" ++ toString (size / 2) ++ "px" )
            ]
         ]
            ++ attributes
        )
        [ elem ]


wrapper : Orientation -> Html msg -> Html msg
wrapper o elem =
    Html.div
        [ Html.style
            [ ( "position", "relative" )
            , ( match { horiz = "width", vert = "height" } o, "100%" )
            ]
        ]
        [ elem ]


defaultDivider : Bool -> Float -> Html msg
defaultDivider hovering size =
    Svg.svg
        [ Attr.width "100%"
        , Attr.height "100%"
        , Attr.viewBox ("0 0 100 " ++ toString (floor size))
        , Attr.preserveAspectRatio "none"
        ]
        [ line hovering 0 (size / 2) 100 (size / 2)

        --, line 0 0 0 20
        --, line 100 0 100 20
        ]


line : Bool -> Float -> Float -> Float -> Float -> Svg msg
line hovering x1 y1 x2 y2 =
    Svg.line
        [ Attr.x1 (toString x1)
        , Attr.y1 (toString y1)
        , Attr.x2 (toString x2)
        , Attr.y2 (toString y2)
        , if hovering then
            Attr.style "stroke:rgb(20,20,20);stroke-width:2"
          else
            Attr.style "stroke:rgb(200,200,200);stroke-width:1"
        ]
        []
