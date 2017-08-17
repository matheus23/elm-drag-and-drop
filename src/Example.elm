module Example exposing (..)

import DragAndDrop
import DragAndDrop.Divider as Divider
import DragAndDrop.ReorderList as ReorderList
import Element exposing (Element)
import Element.Attributes as Element
import Element.Events as Events
import ExampleStyle exposing (..)
import Focus exposing (($=), (&), (.=), (=>), Setter)
import FocusMore as Focus exposing (FieldSetter)
import Html exposing (Html)


-- Model


type alias Item =
    String


type alias Model =
    ReorderList.Model Item


type Msg
    = UpdateItem Int String
    | ReorderListMsg ReorderList.Msg


init : ( Model, Cmd Msg )
init =
    ReorderList.init [ "These", "are", "a", "lot", "of", "draggable", "html", "elements" ] ! []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateItem index newContent ->
            (model & ReorderList.elements => Focus.index index .= newContent) ! []

        ReorderListMsg reorderListMsg ->
            ReorderList.update reorderListMsg model ! []



-- View


viewElement : Model -> Html Msg
viewElement =
    view >> Element.layout stylesheet


view : Model -> Element Style Variants Msg
view model =
    let
        viewItems =
            List.indexedMap (viewItem model.dragModel)

        settings =
            { orientation = Divider.Horizontal
            , dividerSize = 40
            , nostyle = NoStyle
            }
    in
    Element.column ListStyle
        [ Element.width (Element.px 500) ]
        (ReorderList.view settings ReorderListMsg viewItems model)


viewItem : DragAndDrop.Model Int Int -> Int -> Item -> Element Style Variants Msg
viewItem dragModel index item =
    Element.inputText (ItemStyle (DragAndDrop.isDraggingId index dragModel))
        [ Events.onInput (UpdateItem index) ]
        item



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
    Sub.map ReorderListMsg << ReorderList.subscriptions


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewElement
        }
