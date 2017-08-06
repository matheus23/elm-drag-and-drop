module DragAndDrop exposing (..)

import Focus exposing (..)
import FocusMore as Focus
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Decode
import Mouse


-- Model


type alias DraggingData dragId dropId =
    { dragId : dragId
    , hoverDropId : Maybe dropId
    }


type alias NotDraggingData dragId =
    { hoverDragId : Maybe dragId }


type Model dragId dropId
    = Dragging (DraggingData dragId dropId)
    | NotDragging (NotDraggingData dragId)


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
    NotDragging
        { hoverDragId = Nothing }



-- Subscriptions


subscriptions : Model dragId dropId -> Sub (Msg dragId dropId)
subscriptions model =
    if isDragging model then
        Mouse.ups (always StopDragging)
    else
        Sub.none



-- Update


update : Msg dragId dropId -> Model dragId dropId -> Model dragId dropId
update =
    updateHelp False


updateHelp : Bool -> Msg dragId dropId -> Model dragId dropId -> Model dragId dropId
updateHelp sticky msg =
    updateWithEvents sticky msg >> Tuple.first


updateWithEvents : Bool -> Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe (Event dragId dropId) )
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
    case model of
        Dragging _ ->
            True

        NotDragging _ ->
            False


isDraggingId : dragId -> Model dragId dropId -> Bool
isDraggingId droppableId model =
    case model of
        Dragging { dragId } ->
            dragId == droppableId

        NotDragging _ ->
            False


isHoveringDraggableId : dragId -> Model dragId dragId -> Bool
isHoveringDraggableId dragId model =
    case model of
        NotDragging { hoverDragId } ->
            equalsMaybe dragId hoverDragId

        Dragging _ ->
            False


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
    Events.onWithOptions "mousedown"
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
