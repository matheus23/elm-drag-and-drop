module DragAndDrop.ReorderList exposing (..)

import DragAndDrop
import DragAndDrop.Divider as Divider
import Element exposing (Element)
import Element.Attributes as ElementAttr
import Focus exposing (..)
import FocusMore as Focus
import Html.Attributes as Html


type alias Model a =
    { elements : List a
    , dragModel : DragAndDrop.Model Int Int
    }


type Msg msg
    = ElementsMsg msg
    | DragAndDropMsg (DragAndDrop.Msg Int Int)


type alias ViewSettings style variant a msg =
    { nostyle : style
    , dividerSize : Float
    , orientation : Divider.Orientation
    , viewItems : List a -> List (Element style variant msg)
    }


init : List a -> Model a
init elements =
    { elements = elements
    , dragModel = DragAndDrop.init
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
                    DragAndDrop.updateWithEvents False dragAndDropMsg model.dragModel

                possiblyApplyEvents model =
                    Maybe.withDefault model (Maybe.map2 updateDrop event (Just model))
            in
            possiblyApplyEvents (model & dragModel .= newDragModel)


updateDrop : DragAndDrop.Event Int Int -> Model a -> Model a
updateDrop event model =
    case Debug.log "event" event of
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
        removeIndex i =
            Focus.indexConcat i .= []

        insertIndex i toInsert list =
            if i == List.length list then
                list ++ [ toInsert ]
            else
                list & Focus.indexConcat i $= (\e -> [ toInsert, e ])
    in
    -- Dropping above or below the dragged element has no effect
    if dragIndex == dropIndex || dragIndex + 1 == dropIndex then
        list
        -- Be careful to not alter indices by removing or inserting an element
    else if dragIndex > dropIndex then
        list |> removeIndex dragIndex |> insertIndex dropIndex draggedElem
    else
        list |> insertIndex dropIndex draggedElem |> removeIndex dragIndex


subscriptions : Model a -> Sub (Msg msg)
subscriptions model =
    Sub.map DragAndDropMsg (DragAndDrop.subscriptions model.dragModel)



-- View


view : ViewSettings style variant a msg -> Model a -> List (Element style variant (Msg msg))
view settings model =
    let
        divider index =
            Element.html
                (Divider.viewWith (Divider.defaultDivider (DragAndDrop.isHoveringDroppableId index model.dragModel))
                    settings.orientation
                    settings.dividerSize
                    (List.map (Html.map DragAndDropMsg) (DragAndDrop.droppableHtml model.dragModel identity index))
                )

        ( elementCombine, elementAttach, elementAttachBefore ) =
            case settings.orientation of
                Divider.Horizontal ->
                    ( Element.column settings.nostyle []
                    , \a e -> Element.column settings.nostyle [] [ e, a ]
                    , \a e -> Element.column settings.nostyle [] [ a, e ]
                    )

                Divider.Vertical ->
                    ( Element.row settings.nostyle []
                    , \a e -> Element.row settings.nostyle [] [ e, a ]
                    , \a e -> Element.row settings.nostyle [] [ a, e ]
                    )

        addDivider index elem =
            -- No dividers above and below the dragging element needed, dropping there has no effect
            if DragAndDrop.isDraggingId index model.dragModel || DragAndDrop.isDraggingId (index + 1) model.dragModel then
                elem
            else
                elementAttach (divider (index + 1)) elem

        addDividers list =
            if DragAndDrop.isDragging model.dragModel then
                if not (DragAndDrop.isDraggingId 0 model.dragModel) then
                    List.indexedMap addDivider list & Focus.index 0 $= elementAttachBefore (divider 0)
                else
                    List.indexedMap addDivider list
            else
                list

        makeDraggable : Int -> Element style variant msg -> Element style variant (Msg msg)
        makeDraggable index elem =
            Element.el settings.nostyle
                (List.map (ElementAttr.map DragAndDropMsg) (DragAndDrop.draggable model.dragModel identity index))
                (Element.map ElementsMsg elem)
    in
    addDividers (List.indexedMap makeDraggable (settings.viewItems model.elements))



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
