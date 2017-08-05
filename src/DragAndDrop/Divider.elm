module DragAndDrop.Divider exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as Attr


type Orientation
    = Horizontal
    | Vertical


main : Html msg
main =
    Html.ul
        []
        [ Html.li [] [ Html.text "test" ]
        , view Horizontal 20 [] dividerLine
        , Html.li [] [ Html.text "items" ]
        ]


view : Orientation -> Float -> List (Html.Attribute msg) -> Html msg -> Html msg
view orientation size attributes image =
    wrapper orientation <| overlapper orientation size attributes image


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


dividerLine : Html msg
dividerLine =
    Svg.svg
        [ Attr.width "100%"
        , Attr.height "100%"
        , Attr.viewBox "0 0 100 20"
        , Attr.preserveAspectRatio "none"
        ]
        [ line 0 10 100 10

        --, line 0 0 0 20
        --, line 100 0 100 20
        ]


line : Float -> Float -> Float -> Float -> Svg msg
line x1 y1 x2 y2 =
    Svg.line
        [ Attr.x1 (toString x1)
        , Attr.y1 (toString y1)
        , Attr.x2 (toString x2)
        , Attr.y2 (toString y2)
        , Attr.style "stroke:rgb(20,20,20);stroke-width:2"
        ]
        []
