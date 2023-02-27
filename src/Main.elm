module Main exposing (..)

import Browser
import Html exposing (Html, button, div, li, p, text, ul)
import Html.Events exposing (onClick)



-- MODEL


type alias Alarm =
    { armed : Bool
    , triggered : Bool
    }


type alias Door =
    { open : Bool, locked : Bool }


type alias Model =
    { alarm : Alarm, door : Door, message : Maybe String }


init : Model
init =
    { alarm = Alarm False False
    , door = Door False False
    , message = Nothing
    }



-- ACTIONS


type Msg
    = OpenDoor
    | DisarmAlarm
    | ArmAlarm
    | CloseDoor
    | LockDoor
    | UnlockDoor


update msg model =
    case model.door.open of
        True ->
            case msg of
                CloseDoor ->
                    let
                        closeDoor door =
                            { door | open = False }
                    in
                    { model | door = closeDoor model.door, message = Nothing }

                _ ->
                    { model | message = Just "You can't do that when the door's open" }

        False ->
            case msg of
                LockDoor ->
                    if model.door.locked then
                        { model | message = Just "The door is already locked" }

                    else
                        let
                            lockDoor door =
                                { door | locked = True }
                        in
                        { model
                            | door = lockDoor model.door
                            , message = Nothing
                        }

                OpenDoor ->
                    if model.door.locked then
                        { model | message = Just "The door is locked" }

                    else
                        let
                            updateAlarm alarm =
                                { alarm | triggered = alarm.armed }

                            openDoor door =
                                { door | open = True }
                        in
                        { model
                            | alarm = updateAlarm model.alarm
                            , door = openDoor model.door
                            , message = Nothing
                        }

                UnlockDoor ->
                    if model.door.locked then
                        let
                            unlockDoor door =
                                { door | locked = False }
                        in
                        { model
                            | door = unlockDoor model.door
                            , message = Nothing
                        }

                    else
                        { model | message = Just "The door is already unlocked" }

                DisarmAlarm ->
                    { model | alarm = Alarm False False, message = Nothing }

                ArmAlarm ->
                    if model.alarm.triggered then
                        { model | message = Just "The alarm has already been triggered. Disarm it." }

                    else
                        { model | alarm = Alarm True False, message = Nothing }

                CloseDoor ->
                    { model | message = Just "The door is already closed" }



-- VIEW


view model =
    div []
        [ ul []
            [ viewDoor model.door
            , viewAlarm model.alarm
            ]
        , div []
            [ button [ onClick CloseDoor ] [ text "Close door" ]
            , button [ onClick OpenDoor ] [ text "Open door" ]
            , button [ onClick ArmAlarm ] [ text "Arm the alarm" ]
            , button [ onClick DisarmAlarm ] [ text "Disarm the alarm" ]
            , button [ onClick LockDoor ] [ text "Lock the door" ]
            , button [ onClick UnlockDoor ] [ text "unlock the door" ]
            ]
        , viewMessage model
        ]


viewDoor : Door -> Html Msg
viewDoor door =
    let
        openStatus =
            if door.open then
                "Open"

            else
                "Closed"

        lockedStatus =
            if door.locked then
                "Locked"

            else
                "Unlocked"
    in
    li [] [ text ("The door is " ++ openStatus ++ " and " ++ lockedStatus) ]


viewAlarm : Alarm -> Html Msg
viewAlarm alarm =
    let
        alarmStatus =
            if alarm.triggered then
                "TRIGGERED"

            else
                case alarm.armed of
                    True ->
                        "armed"

                    False ->
                        "disarmed"
    in
    li [] [ text ("The alarm is " ++ alarmStatus) ]


viewMessage : Model -> Html Msg
viewMessage model =
    case model.message of
        Nothing ->
            text ""

        Just messageText ->
            p [] [ text messageText ]


main =
    Browser.sandbox { init = init, update = update, view = view }
