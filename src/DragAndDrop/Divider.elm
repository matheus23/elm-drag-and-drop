module DragAndDrop.Divider
    exposing
        ( Orientation(..)
        , defaultDivider
        , viewWith
        )

{-| This module's contents are almost all unexposed right now. This might change in the future,
when you can customize the dividers in your [`ReorderList`](DragAndDrop-ReorderList).

    import DragAndDrop.Divider as Divider

@docs Orientation


# Unstable API

@docs viewWith, defaultDivider

-}

import Html exposing (Html)
import Html.Attributes as Html
import Svg exposing (Svg)
import Svg.Attributes as Attr


{-| The orientation the divider is dividing.

Think of a divider as a fine line between elements. It is a horizontal divider,
when the line is horizontal (even though it divides elements vertically).

This might be really confusing right now can should change in the future.

-}
type Orientation
    = Horizontal
    | Vertical


type alias ViewFunction msg =
    Orientation -> Float -> List (Html.Attribute msg) -> Html msg


type alias DividerImage msg =
    Bool -> Orientation -> Float -> Html msg


view : Html msg -> ViewFunction msg
view divider orientation size attributes =
    wrapper orientation <| overlapper orientation size attributes divider


viewWithSize : (Float -> Html msg) -> ViewFunction msg
viewWithSize viewDivider orientation size =
    view (viewDivider size) orientation size


{-| View a divider

    Divider.viewWith (Divider.defaultDivider isHovered) orientation size attributes

Api might change in the future.

-}
viewWith : (Orientation -> Float -> Html msg) -> ViewFunction msg
viewWith viewDivider orientation size =
    view (viewDivider orientation size) orientation size


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
            , ( "z-index", "100" )
            ]
        ]
        [ elem ]


{-| A default divider.

Shows a fine, black line when hovered and a fine grey line when not hovered.

The line is oriented the way given as orientation.

Api might change in the future.

-}
defaultDivider : DividerImage msg
defaultDivider hovering orientation size =
    Svg.svg
        [ Attr.width "100%"
        , Attr.height "100%"
        , Attr.viewBox
            (match
                { horiz = "0 0 " ++ toString (floor (size * 5)) ++ " " ++ toString (floor size)
                , vert = "0 0 " ++ toString (floor size) ++ " " ++ toString (floor (size * 5))
                }
                orientation
            )
        , Attr.preserveAspectRatio "none"
        ]
        [ match
            { horiz = line hovering 0 (size / 2) (size * 5) (size / 2)
            , vert = line hovering (size / 2) 0 (size / 2) (size * 5)
            }
            orientation
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
