module DragAndDrop.ReorderList exposing (..)

import DragAndDrop
import DragAndDrop.Divider as Divider
import Focus exposing (..)
import FocusMore as Focus
import Html exposing (Html)
import Html.Attributes as Attributes


type alias Model a =
    { elements : List a
    , dragModel : DragAndDrop.Model Int Int
    , orientation : Divider.Orientation
    }


type Msg msg
    = ElementsMsg msg
    | DragAndDropMsg (DragAndDrop.Msg Int Int)


init : Divider.Orientation -> List a -> Model a
init orientation elements =
    { elements = elements
    , dragModel = DragAndDrop.init
    , orientation = orientation
    }



-- Update


update : (msg -> List a -> List a) -> Msg msg -> Model a -> Model a
update updateOthers msg model =
    case msg of
        ElementsMsg elementsMsg ->
            model & elements $= updateOthers elementsMsg

        DragAndDropMsg dragAndDropMsg ->
            let
                ( newDragModel, event ) =
                    DragAndDrop.updateWithEvents dragAndDropMsg model.dragModel

                possiblyApplyEvents model =
                    Maybe.withDefault model (Maybe.map2 updateDrop event (Just model))
            in
            possiblyApplyEvents (model & dragModel .= newDragModel)


updateDrop : DragAndDrop.Event Int Int -> Model a -> Model a
updateDrop event model =
    case event of
        DragAndDrop.SuccessfulDrop dragIndex dropIndex ->
            Maybe.withDefault model
                (Maybe.map
                    (\draggedElem ->
                        model & elements $= applyDrop dragIndex dropIndex draggedElem
                    )
                    (getIndex dragIndex model.elements)
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



-- View


view : Html (Msg msg) -> (List a -> List (Html msg)) -> Model a -> List (Html (Msg msg))
view viewDroppable viewInner model =
    let
        divider index =
            Divider.view Divider.Horizontal
                20
                (List.map (Attributes.map DragAndDropMsg) (DragAndDrop.droppable model.dragModel identity index))
                viewDroppable

        addDivider index elem =
            [ Html.map ElementsMsg elem, divider (index + 1) ]

        addDividers list =
            if DragAndDrop.isDragging model.dragModel then
                divider 0 :: List.concat (List.indexedMap addDivider list)
            else
                List.map (Html.map ElementsMsg) list

        makeDraggable : Int -> Html (Msg msg) -> Html (Msg msg)
        makeDraggable index elem =
            Html.div
                (List.map (Attributes.map DragAndDropMsg) (DragAndDrop.draggable model.dragModel identity index))
                [ elem ]
    in
    addDividers (List.indexedMap makeDraggable (viewInner model.elements))



-- Lenses


elements : Focus.Setter (Model a) (Model b) (List a) (List b)
elements f model =
    { model | elements = f model.elements }


dragModel : Focus.FieldSetter (Model a) (DragAndDrop.Model Int Int)
dragModel f model =
    { model | dragModel = f model.dragModel }



-- Utilities


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
