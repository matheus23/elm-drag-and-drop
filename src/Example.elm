module Example exposing (..)

import DragAndDrop
import DragAndDrop.Divider as Divider
import DragAndDrop.ReorderList as ReorderList
import Focus exposing (..)
import FocusMore as Focus
import Html exposing (Html)
import Html.Attributes as Html


-- Model


type alias Item =
    String


type alias Model =
    ReorderList.Model Item


type alias Msg =
    ReorderList.Msg Never


init : ( Model, Cmd Msg )
init =
    ReorderList.init Divider.Horizontal [ "These", "are", "a", "lot", "of", "draggable", "html", "elements" ] ! []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ReorderList.update (\_ m -> m) msg model ! []



-- View


view : Model -> Html Msg
view model =
    let
        droppableImage hovering size =
            Html.div
                [ Html.style
                    [ ( "width", "inherit" ), ( "height", "inherit" ) ]
                ]
                [ Divider.defaultDivider hovering size ]

        viewItems items =
            List.indexedMap (viewItem model.dragModel) items
    in
    Html.ul
        []
        (ReorderList.view 40 droppableImage viewItems model)


viewItem : DragAndDrop.Model Int Int -> Int -> Item -> Html msg
viewItem dragModel index item =
    let
        attributes =
            [ Html.style [ ( "font-size", "40px" ) ] ]
                |> appendWhen (DragAndDrop.isDraggingId index dragModel)
                    [ Html.style [ ( "color", "grey" ) ] ]
    in
    Html.li attributes [ Html.text item ]



-- Util


appendWhen : Bool -> List a -> List a -> List a
appendWhen b toAppend list =
    if b then
        list ++ toAppend
    else
        list



-- Main


subscriptions : Model -> Sub Msg
subscriptions =
    ReorderList.subscriptions


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
