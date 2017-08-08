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


type ItemsMsg
    = UpdateItem Int String


type alias Msg =
    ReorderList.Msg ItemsMsg


init : ( Model, Cmd Msg )
init =
    ReorderList.init [ "These", "are", "a", "lot", "of", "draggable", "html", "elements" ] ! []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ReorderList.update updateItems msg model ! []


updateItems : ItemsMsg -> List Item -> List Item
updateItems msg items =
    case msg of
        UpdateItem index newContent ->
            items & Focus.index index .= newContent



-- View


viewElement : Model -> Html Msg
viewElement =
    view >> Element.layout stylesheet


view : Model -> Element Style Variants Msg
view model =
    let
        settings =
            { viewItems =
                \items ->
                    List.indexedMap (viewItem model.dragModel) items
            , orientation = Divider.Horizontal
            , dividerSize = 40
            , nostyle = NoStyle
            }
    in
    Element.column ListStyle
        [ Element.width (Element.px 500) ]
        (ReorderList.view settings model)


viewItem : DragAndDrop.Model Int Int -> Int -> Item -> Element Style Variants ItemsMsg
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
    ReorderList.subscriptions


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewElement
        }
