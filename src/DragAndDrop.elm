module DragAndDrop
    exposing
        ( DraggingData
        , Event(..)
        , Model(..)
        , Msg
        , NotDraggingData
        , draggable
        , draggableHtml
        , droppable
        , droppableHtml
        , init
        , isDragging
        , isDraggingId
        , isHoveringDraggableId
        , isHoveringDroppableId
        , subscriptions
        , update
        , updateWithEvents
        )

{-|


# DragAndDrop

This model is the low-level interface for drag and drop. It can still directly be
useful for applications, though.


# Model

@docs Model, DraggingData, NotDraggingData, init


# Messages

@docs Msg, Event


# Updating

@docs updateWithEvents, update, subscriptions


# Make Elements draggable/droppable in your View

@docs draggable, droppable, draggableHtml, droppableHtml


# Querying the Dragging Model

@docs isDragging, isDraggingId, isHoveringDraggableId, isHoveringDroppableId

-}

import Element exposing (Element)
import Element.Attributes as Attr
import Focus exposing (..)
import FocusMore as Focus
import Html exposing (Html)
import Html.Events as HtmlEvents
import Json.Decode as Decode
import Mouse


-- Model


{-| This is the Model that is present during a Dragging action. The `dragId`
is the element that was dragged, and the `hoverDropId` is `Just dropId`, if
the user is currently dragging above a droppable element, but has not released
the mouse yet, or `Nothing` if not hovering any droppable element.

Droppable elements are elements with attributes from the functions
`droppable` or `droppableHtml`.

-}
type alias DraggingData dragId dropId =
    { dragId : dragId
    , hoverDropId : Maybe dropId
    }


{-| This is the Model that is present while no element is being dragged.
In this case `hoverDragId` stores `Just dragId` if the mouse is currently
over a draggable element (see `draggable` and `draggableHtml`), but the user
did not yet started to drag the element.
-}
type alias NotDraggingData dragId =
    { hoverDragId : Maybe dragId }


{-| The model for the low level drag-and-drop api. It is ment to be stored
inside the model of the component using this module.

Along the lines of this:

    type alias Model =
        { draggableCatImages : List CatIdentifier
        , droppableCatBaskets : List (List CatIdentifier)
        , dragModel : DragAndDrop.Model CatIdentifier Int
        }

Where the `dragId` is the `CatIdentifier` used to find out what cat image
was dragged by the user, and `dropId` is an `Int` for identifying the
basked that the cat image was dropped to.

-}
type Model dragId dropId
    = Dragging (DraggingData dragId dropId)
    | NotDragging (NotDraggingData dragId)


{-| The messages for this module. Include them in your Msg type like so:

    type Msg
        = ...
        | DragAndDropMsg (DragAndDrop.Msg CatIdentifier Int)

-}
type Msg dragId dropId
    = EnterDraggable dragId
    | LeaveDraggable dragId
    | EnterDroppable dropId
    | LeaveDroppable dropId
    | StartDragging dragId
    | StopDragging


{-| Events that can be produced by the `updateWithEvents` function after
a drop action, that is a user hovering a draggable element, starting to
drag the mouse and then releasing the mouse.

If the user has released the mouse while hovering a droppable element,
then a `SuccessfulDrop dragId dropId`, if not, a
`FailedDrop dragId` is generated.

Upon starting to drag a draggable element, a `StartedDrag dragId` event is
produced.

-}
type Event dragId dropId
    = StartedDrag dragId
    | SuccessfulDrop dragId dropId
    | FailedDrop dragId


{-| The initial model for this module's `Model`
-}
init : Model dragId dropId
init =
    NotDragging
        { hoverDragId = Nothing }



-- Subscriptions


{-| You need to include the subscriptions for this project in order for
drop events to be registered, since this listens on `Mouse.ups`.

Add it to your subscriptions like this:

    subscriptions : Sub Msg
    subscriptions model =
        Sub.batch
            [ ...
            , Sub.map DragAndDropMsg (DragAndDrop.subscriptions model.dragModel)
            ]

-}
subscriptions : Model dragId dropId -> Sub (Msg dragId dropId)
subscriptions model =
    if isDragging model then
        Mouse.ups (always StopDragging)
    else
        Sub.none



-- Update


{-| Similar to `updateWithEvents` but non-sticky by default and without
producing events.

In your own update function:

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            DragAndDropMsg dragAndDropMsg ->
                { model | dragModel = DragAndDrop.update dragAndDropMsg model.dragModel }

            ... -> ...

-}
update : Msg dragId dropId -> Model dragId dropId -> Model dragId dropId
update =
    updateHelp False


updateHelp : Bool -> Msg dragId dropId -> Model dragId dropId -> Model dragId dropId
updateHelp sticky msg =
    updateWithEvents sticky msg >> Tuple.first


{-| Use this method in your update function to receive `Event`s if
the user sucessfully drag-and-dropped an element or failed to do so, etc.
(see `Event`).

Updating can be done sticky, that means a drop is successful, even if the user
does not hover the droppable area anymore, but has done so before.

Use it in your update function:

    update : Msg -> Model -> Model
    update msg model =
        case DragAndDropMsg dndMsg ->
            let ( dragModel, maybeEvent ) =
                    updateWithEvents True dndMsg model.dragModel

                newModel = { model | dragModel = dragModel }
            in
            Maybe.withDefault newModel (Maybe.map maybeEvent (updateDrop newModel))

    updateDrop : DragAndDrop.Event CatIdentifier Int -> Model -> Model
    updateDrop event = ...

-}
updateWithEvents :
    Bool
    -> Msg dragId dropId
    -> Model dragId dropId
    -> ( Model dragId dropId, Maybe (Event dragId dropId) )
updateWithEvents sticky msg model =
    let
        replaceNothingIfEqual value maybe =
            case maybe of
                Just sth ->
                    if value == sth then
                        Nothing
                    else
                        Just sth

                Nothing ->
                    Nothing
    in
    case msg of
        EnterDraggable dragId ->
            ( model & notDragging => hoverDragId .= Just dragId, Nothing )

        LeaveDraggable dragId ->
            ( model & notDragging => hoverDragId $= replaceNothingIfEqual dragId, Nothing )

        EnterDroppable dropId ->
            ( model & dragging => hoverDropId .= Just dropId, Nothing )

        LeaveDroppable dropId ->
            ( model
                |> Focus.when (not sticky)
                    (dragging => hoverDropId $= replaceNothingIfEqual dropId)
            , Nothing
            )

        StartDragging dragId ->
            if not (isDragging model) then
                ( Dragging
                    { dragId = dragId
                    , hoverDropId = Nothing
                    }
                , Just (StartedDrag dragId)
                )
            else
                ( model, Nothing )

        StopDragging ->
            let
                dropEvent dragId =
                    case model of
                        Dragging { hoverDropId } ->
                            case hoverDropId of
                                Just dropId ->
                                    SuccessfulDrop dragId dropId

                                _ ->
                                    FailedDrop dragId

                        _ ->
                            FailedDrop dragId
            in
            ( NotDragging { hoverDragId = Nothing }
            , case model of
                Dragging { dragId } ->
                    Just (dropEvent dragId)

                _ ->
                    Nothing
            )



-- Attributes


{-| A version of `draggable` for usage without the style-elements package, but with
elm-lang/html.
-}
draggableHtml : Model dragId dropId -> (Msg dragId dropId -> msg) -> dragId -> List (Html.Attribute msg)
draggableHtml model inject dragId =
    if not (isDragging model) then
        [ HtmlEvents.onMouseOver (inject (EnterDraggable dragId))
        , HtmlEvents.onMouseLeave (inject (LeaveDraggable dragId))
        , preventingOnMouseDown (inject (StartDragging dragId))
        ]
    else
        []


{-| A version of `droppable` for usage without the style-elements package, but with
elm-lang/html.
-}
droppableHtml : Model dragId dropId -> (Msg dragId dropId -> msg) -> dropId -> List (Html.Attribute msg)
droppableHtml model inject dropId =
    if isDragging model then
        [ HtmlEvents.onMouseOver (inject (EnterDroppable dropId))
        , HtmlEvents.onMouseLeave (inject (LeaveDroppable dropId))

        -- we find out on what droppable was dropped on by model.droppableHover
        ]
    else
        []


{-| Make a style-element `Element` draggable in your view by appending these attributes:

    viewCatImage : Model -> CatIdentifier -> Element Style Variation Msg
    viewCatImage model identifier =
        Element.el Style
            (DragAndDrop.draggable model.dragModel DragAndDropMsg identifier)
            (viewImage model identifier)

-}
draggable : Model dragId dropId -> (Msg dragId dropId -> msg) -> dragId -> List (Element.Attribute varying msg)
draggable model inject dragId =
    List.map Attr.toAttr (draggableHtml model inject dragId)


{-| Make a style-element `Element` droppable in your view by appending these attributes:

    viewBasket : Model -> Int -> Element Style Variation Msg
    viewBasket model basketIndex =
        Element.el Style
            (DragAndDrop.droppable model.dragModel DragAndDropMsg basketIndex)
            renderedBasket

-}
droppable : Model dragId dropId -> (Msg dragId dropId -> msg) -> dropId -> List (Element.Attribute varying msg)
droppable model inject dropId =
    List.map Attr.toAttr (droppableHtml model inject dropId)



-- Querying


{-| Find out whether the user is currently dragging an element.
-}
isDragging : Model dragId dropId -> Bool
isDragging model =
    case model of
        Dragging _ ->
            True

        NotDragging _ ->
            False


{-| Find out whether the user is currently dragging a specific element
-}
isDraggingId : dragId -> Model dragId dropId -> Bool
isDraggingId droppableId model =
    case model of
        Dragging { dragId } ->
            dragId == droppableId

        NotDragging _ ->
            False


{-| Find out wheter the user is currently hovering over a specific draggable
element (while not dragging)
-}
isHoveringDraggableId : dragId -> Model dragId dragId -> Bool
isHoveringDraggableId dragId model =
    case model of
        NotDragging { hoverDragId } ->
            equalsMaybe dragId hoverDragId

        Dragging _ ->
            False


{-| Find out whether the user is currently hovering over a specific droppable
element (while dragging)
-}
isHoveringDroppableId : dropId -> Model dragId dropId -> Bool
isHoveringDroppableId dropId model =
    case model of
        Dragging { hoverDropId } ->
            equalsMaybe dropId hoverDropId

        NotDragging _ ->
            False



-- Internal


equalsMaybe : a -> Maybe a -> Bool
equalsMaybe a maybe =
    Maybe.withDefault False (Maybe.map ((==) a) maybe)


preventingOnMouseDown : msg -> Html.Attribute msg
preventingOnMouseDown msg =
    HtmlEvents.onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Decode.succeed msg)



-- Lenses


dragging : Focus.FieldSetter (Model dragId dropId) (DraggingData dragId dropId)
dragging f model =
    case model of
        Dragging data ->
            Dragging (f data)

        _ ->
            model


notDragging : Focus.FieldSetter (Model dragId dropId) (NotDraggingData dragId)
notDragging f model =
    case model of
        NotDragging data ->
            NotDragging (f data)

        _ ->
            model


hoverDragId : Focus.FieldSetter (NotDraggingData dragId) (Maybe dragId)
hoverDragId f model =
    { model | hoverDragId = f model.hoverDragId }


hoverDropId : Focus.FieldSetter (DraggingData dragId dropId) (Maybe dropId)
hoverDropId f model =
    { model | hoverDropId = f model.hoverDropId }
