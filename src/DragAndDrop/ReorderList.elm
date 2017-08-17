module DragAndDrop.ReorderList
    exposing
        ( Event
        , Model
        , Msg(..)
        , ViewConfig
        , elements
        , init
        , subscriptions
        , update
        , updateWithEvents
        , view
        , viewKeyed
        )

{-|


# Make a reorderable list of elements

This module is ment to be imported like this:

    import DragAndDrop.ReorderList as ReorderList
    import DragAndDrop.Divider as Divider

Using this module you can simply refactor your `List a` of non-reorderable
elements in your model to reorderable elements, by changing the type to
[`ReorderList.Model a`](DragAndDrop-ReorderList#Model) and adding some
update functions, subscriptions and messages.


# Model

@docs Model, init, Msg, Event


# Update

@docs update, updateWithEvents, subscriptions


# View

@docs ViewConfig, view, viewKeyed


# Model utilities

@docs elements

-}

import DragAndDrop
import DragAndDrop.Divider as Divider
import Element exposing (Element)
import Element.Attributes as ElementAttr
import Focus exposing (..)
import FocusMore as Focus
import Html.Attributes as Html


{-| The model for a list of reorderable elements.

You can use this as a replacement for lists in your model
(which werent reorderable before). So if you have this:

    type alias Model =
        { catImages : List CatImage
        }

You can refactor it to this:

    type alias Model =
        { catImages : ReorderList.Model CatImage
        }

Use [`init`](#init) to create an initial model.

Maintains the [`DragAndDrop.Model`](DragAndDrop#Model) for making use of the
[`DragAndDrop`](DragAndDrop)-module.

-}
type alias Model a =
    { elements : List a
    , dragModel : DragAndDrop.Model Int Int
    }


{-| This is the type of messages this list can create. Include it in your
message union type like so:

    type Msg
        = UpdateCatImage Int
        | ReorderListMsg ReorderList.Msg

-}
type Msg
    = DragAndDropMsg (DragAndDrop.Msg Int Int)


{-| The alias for the type of DragAndDrop-event that the underlying
low-level api produces. It's just a shorthand.
See [`DragAndDrop.Event`](DragAndDrop#Event).
-}
type alias Event =
    DragAndDrop.Event Int Int


{-| A configuration for the viewing function. It should be the same
for all of your calls, so could be top-level defined.

    reorderListConfig : ViewConfig MyStyle MyVariants Msg
    reorderListConfig =
        { nostyle = MyNoStyle -- your representation of no styles for an element
        , dividerSize = 40 -- size for the dividers between elements, that can be dropped to in pixels
        , orientation = Divider.Horizontal -- the orientation the the *dividers* have (not the overall list)
        }

-}
type alias ViewConfig style =
    { nostyle : style
    , dividerSize : Float
    , orientation : Divider.Orientation
    }


{-| Create an initial [`ReorderList.Model`](#Model) from a list of elements.
-}
init : List a -> Model a
init elements =
    { elements = elements
    , dragModel = DragAndDrop.init
    }



-- Update


{-| The updating function that updates the model when it's given
[`Msg`](#Msg)s.

In your update function:

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            UpdateCatImage index ->
                ...

            ReorderListMsg reorderListMsg ->
                { model | catImages = ReorderList.update reorderListMsg model.catImages }

-}
update : Msg -> Model a -> Model a
update msg model =
    Tuple.first (updateWithEvents msg model)


{-| Similar to [`update`](#update), but also gives information about changes
in the drag-and-drop state. See [`DragAndDrop.Event`](DragAndDrop#Event)
-}
updateWithEvents : Msg -> Model a -> ( Model a, Maybe Event )
updateWithEvents msg model =
    case msg of
        DragAndDropMsg dragAndDropMsg ->
            let
                ( newDragModel, event ) =
                    DragAndDrop.updateWithEvents False dragAndDropMsg model.dragModel

                possiblyApplyEvents model =
                    Maybe.withDefault model (Maybe.map2 updateDrop event (Just model))
            in
            ( possiblyApplyEvents (model & dragModel .= newDragModel), event )


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


{-| The subscriptions necessary for running this module and generating needed
messages for drag and drop actions.
-}
subscriptions : Model a -> Sub Msg
subscriptions model =
    Sub.map DragAndDropMsg (DragAndDrop.subscriptions model.dragModel)



-- View


{-| View a reorderable list of elements, but without combining them all together. You get
back a `List (Element style variant msg)`. You can work with this list however you
want to. Only show the first 10 elements, or only the last 7, or you can interleave these
elements with other `Element`s.

You need to provide a [`ViewConfig`](#ViewConfig), a way to inject messages into your message
union type, a viewing function for elements and a [`ReorderList.Model`](#Model).

This already attaches dividers when dragging elements.

-}
view :
    ViewConfig style
    -> (Msg -> msg)
    -> (List a -> List (Element style variant msg))
    -> Model a
    -> List (Element style variant msg)
view settings injectMsg viewItems model =
    let
        dragAndDropMsg =
            DragAndDropMsg >> injectMsg

        divider index =
            Element.html
                (Divider.viewWith
                    (Divider.defaultDivider (DragAndDrop.isHoveringDroppableId index model.dragModel))
                    settings.orientation
                    settings.dividerSize
                    (List.map (Html.map dragAndDropMsg) (DragAndDrop.droppableHtml model.dragModel identity index))
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

        makeDraggable index elem =
            Element.el settings.nostyle
                (List.map (ElementAttr.map dragAndDropMsg) (DragAndDrop.draggable model.dragModel identity index))
                elem
    in
    addDividers (List.indexedMap makeDraggable (viewItems model.elements))


{-| Keyed version of [`view`](#view). Helps the virtualdom to figure out a better mapping from past to
present elements.
-}
viewKeyed :
    ViewConfig style
    -> (Msg -> msg)
    -> (List a -> List ( String, Element style variant msg ))
    -> Model a
    -> List ( String, Element style variant msg )
viewKeyed settings injectMsg viewItems model =
    -- copy paste programming, weeeee
    let
        dragAndDropMsg =
            DragAndDropMsg >> injectMsg

        divider index =
            Element.html
                (Divider.viewWith
                    (Divider.defaultDivider (DragAndDrop.isHoveringDroppableId index model.dragModel))
                    settings.orientation
                    settings.dividerSize
                    (List.map (Html.map dragAndDropMsg) (DragAndDrop.droppableHtml model.dragModel identity index))
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

        addDivider index ( key, elem ) =
            -- No dividers above and below the dragging element needed, dropping there has no effect
            if DragAndDrop.isDraggingId index model.dragModel || DragAndDrop.isDraggingId (index + 1) model.dragModel then
                ( key, elem )
            else
                ( key, elementAttach (divider (index + 1)) elem )

        addDividers list =
            if DragAndDrop.isDragging model.dragModel then
                if not (DragAndDrop.isDraggingId 0 model.dragModel) then
                    List.indexedMap addDivider list & Focus.index 0 => Focus.second $= elementAttachBefore (divider 0)
                else
                    List.indexedMap addDivider list
            else
                list

        makeDraggable index ( key, elem ) =
            ( key
            , Element.el settings.nostyle
                (List.map (ElementAttr.map dragAndDropMsg) (DragAndDrop.draggable model.dragModel identity index))
                elem
            )
    in
    addDividers (List.indexedMap makeDraggable (viewItems model.elements))



-- Lenses


{-| Focus on a subpart of the model, the list of elements.
-}
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
