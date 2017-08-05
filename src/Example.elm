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
    ReorderList.Msg ()


init : ( Model, Cmd Msg )
init =
    ReorderList.init Divider.Horizontal [ "These", "are", "draggable" ] ! []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( dragModel, event ) =
            DragAndDrop.updateWithEvents msg model.dragModel

        possiblyApplyEvents model =
            Maybe.withDefault model (Maybe.map2 updateDrop event (Just model))
    in
    possiblyApplyEvents { model | dragModel = dragModel } ! []


updateDrop : DragAndDrop.Event Int Int -> Model -> Model
updateDrop event model =
    case event of
        DragAndDrop.SuccessfulDrop dragIndex dropIndex ->
            --model & items $= switchIndices dragIndex dropIndex
            Maybe.withDefault model
                (Maybe.map
                    (\draggedElem ->
                        model & items $= applyDrop dragIndex dropIndex draggedElem
                    )
                    (getIndex dragIndex model.items)
                )

        _ ->
            model


applyDrop : Int -> Int -> a -> List a -> List a
applyDrop dragIndex dropIndex draggedElem list =
    let
        handleIndex index elem =
            if index == 0 && dropIndex == 0 then
                [ draggedElem, elem ]
            else if index + 1 == dropIndex then
                [ elem, draggedElem ]
            else if index == dragIndex then
                []
            else
                [ elem ]
    in
    List.concat (List.indexedMap handleIndex list)


orTry : Maybe a -> Maybe a -> Maybe a
orTry maybe1 maybe2 =
    case maybe1 of
        Just x ->
            Just x

        Nothing ->
            maybe2


getIndex : Int -> List a -> Maybe a
getIndex i =
    let
        justOnEqual index =
            if index == i then
                Just
            else
                always Nothing
    in
    List.foldr orTry Nothing << List.indexedMap justOnEqual



-- View


view : Model -> Html Msg
view model =
    let
        droppableImage =
            Html.div
                [ Html.style
                    [ ( "width", "inherit" ), ( "height", "inherit" ) ]
                ]
                [ Divider.dividerLine ]

        viewItems items =
            List.indexedMap (viewItem model.dragModel) items
    in
    Html.ul
        []
        (ReorderList.view droppableImage viewItems model)


viewItem : DragAndDrop.Model Int Int -> Int -> Item -> Html msg
viewItem dragModel index item =
    let
        attributes =
            DragAndDrop.draggable dragModel identity index
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
subscriptions model =
    DragAndDrop.subscriptions model.dragModel


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Lenses


items : Focus.FieldSetter Model (List Item)
items f model =
    { model | items = f model.items }


dragModel : Focus.FieldSetter Model (DragAndDrop.Model Int Int)
dragModel f model =
    { model | dragModel = f model.dragModel }
