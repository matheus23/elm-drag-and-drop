module DragAndDrop exposing (..)

import Focus exposing (..)
import FocusMore as Focus
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Decode
import Mouse


-- Model


type alias Model dragId dropId =
    { dragging : Maybe dragId
    , draggableHover : Maybe dragId
    , droppableHover : Maybe dropId
    }


type Msg dragId dropId
    = EnterDraggable dragId
    | LeaveDraggable dragId
    | EnterDroppable dropId
    | LeaveDroppable dropId
    | StartDragging dragId
    | StopDragging


type Event dragId dropId
    = StartedDrag dragId
    | SuccessfulDrop dragId dropId
    | FailedDrop dragId


init : Model dragId dropId
init =
    { dragging = Nothing, draggableHover = Nothing, droppableHover = Nothing }



-- Subscriptions


subscriptions : Model dragId dropId -> Sub (Msg dragId dropId)
subscriptions model =
    if isDragging model then
        Mouse.ups (always StopDragging)
    else
        Sub.none



-- Update


update : Msg dragId dropId -> Model dragId dropId -> Model dragId dropId
update msg =
    updateWithEvents msg >> Tuple.first


updateWithEvents : Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe (Event dragId dropId) )
updateWithEvents msg model =
    case msg of
        EnterDraggable dragId ->
            ( model & draggableHover .= Just dragId, Nothing )

        LeaveDraggable dragId ->
            -- because you can never be quite sure about the order of events.
            ( model
                |> Focus.when (equalsMaybe dragId model.draggableHover)
                    (draggableHover .= Nothing)
            , Nothing
            )

        EnterDroppable dropId ->
            ( model & droppableHover .= Just dropId, Nothing )

        LeaveDroppable dropId ->
            ( model
                |> Focus.when (equalsMaybe dropId model.droppableHover)
                    (droppableHover .= Nothing)
            , Nothing
            )

        StartDragging dragId ->
            ( model
                |> (dragging .= Just dragId)
                |> (draggableHover .= Nothing)
            , Just (StartedDrag dragId)
            )

        StopDragging ->
            let
                dropEvent dragId =
                    case model.droppableHover of
                        Just dropId ->
                            SuccessfulDrop dragId dropId

                        Nothing ->
                            FailedDrop dragId
            in
            ( model
                |> (dragging .= Nothing)
                |> (droppableHover .= Nothing)
            , Maybe.map dropEvent model.dragging
            )



-- Attributes


draggable : Model dragId dropId -> (Msg dragId dropId -> msg) -> dragId -> List (Html.Attribute msg)
draggable model inject dragId =
    if not (isDragging model) then
        [ Events.onMouseEnter (inject (EnterDraggable dragId))
        , Events.onMouseLeave (inject (LeaveDraggable dragId))
        , preventingOnMouseDown (inject (StartDragging dragId))
        ]
    else
        []


droppable : Model dragId dropId -> (Msg dragId dropId -> msg) -> dropId -> List (Html.Attribute msg)
droppable model inject dropId =
    if isDragging model then
        [ Events.onMouseEnter (inject (EnterDroppable dropId))
        , Events.onMouseLeave (inject (LeaveDroppable dropId))

        -- we find out on what droppable was dropped on by model.droppableHover
        ]
    else
        []



-- Querying


isDragging : Model dragId dropId -> Bool
isDragging model =
    isJust model.dragging


isDraggingId : dragId -> Model dragId dropId -> Bool
isDraggingId dragId model =
    equalsMaybe dragId model.dragging


isHoveringDraggableId : dragId -> Model dragId dragId -> Bool
isHoveringDraggableId dragId model =
    equalsMaybe dragId model.draggableHover


isHoveringDroppableId : dropId -> Model dragId dropId -> Bool
isHoveringDroppableId dropId model =
    equalsMaybe dropId model.droppableHover



-- Internal


equalsMaybe : a -> Maybe a -> Bool
equalsMaybe a maybe =
    Maybe.withDefault False (Maybe.map ((==) a) maybe)


isJust : Maybe a -> Bool
isJust m =
    Maybe.withDefault False (Maybe.map (always True) m)


preventingOnMouseDown : msg -> Html.Attribute msg
preventingOnMouseDown msg =
    Events.onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Decode.succeed msg)



-- Lenses


dragging : Focus.FieldSetter (Model dragId dropId) (Maybe dragId)
dragging f model =
    { model | dragging = f model.dragging }


draggableHover : Focus.FieldSetter (Model dragId dropId) (Maybe dragId)
draggableHover f model =
    { model | draggableHover = f model.draggableHover }


droppableHover : Focus.FieldSetter (Model dragId dropId) (Maybe dropId)
droppableHover f model =
    { model | droppableHover = f model.droppableHover }
