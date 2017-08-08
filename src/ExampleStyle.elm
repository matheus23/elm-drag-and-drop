module ExampleStyle exposing (..)

import Color
import Style
import Style.Color as Color
import Style.Font as Font


type Style
    = NoStyle
    | ListStyle
    | ItemStyle Bool


type alias Variants =
    ()


stylesheet : Style.StyleSheet Style Variants
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style ListStyle []
        , Style.style (ItemStyle False)
            [ Font.size 40 ]
        , Style.style (ItemStyle True)
            [ Font.size 40
            , Color.text Color.grey
            ]
        ]
