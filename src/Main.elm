module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events
    exposing
        ( onAnimationFrame
        , onMouseDown
        , onMouseMove
        , onMouseUp
        )
import Browser.Navigation as Navigation
import Game.Character exposing (Character)
import Game.Ui as Ui exposing (Intent(..))
import Element
import Html
import Json.Decode as Decode exposing (Decoder, Value)
import Task
import Time exposing (Posix)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = Fact << UrlChanged
        , onUrlRequest = Fact << LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view =
            \model ->
                let
                    { title, body } =
                        view model
                in
                { title = title
                , body = List.map (Html.map Intent) body
                }
        }


decodeMouseCoord : (MouseCoord -> msg) -> Decoder msg
decodeMouseCoord msgFromCoord =
    Decode.map2 MouseCoord
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        |> Decode.map msgFromCoord


subscriptions : Model -> Sub Msg
subscriptions { isMouseDown } =
    Sub.batch
        [ onAnimationFrame (Fact << AnimationFrameElapsed)
        , onMouseDown (decodeMouseCoord (Fact << MouseHeldDown))
        , if isMouseDown then
            onMouseMove (decodeMouseCoord (Fact << MouseMovedWhileHeldDown))
          else
            Sub.none
        , onMouseUp (decodeMouseCoord (Fact << MouseReleased))
        ]


type alias Model =
    { before : Posix
    , isMouseDown : Bool
    , lastPosition : MouseCoord
    , mode : GameMode
    , navKey : Navigation.Key
    , now : Posix
    , position : MouseCoord
    , startTime : Posix
    , state : GameState
    , target : MouseCoord
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url navKey =
    { before = Time.millisToPosix 0
    , isMouseDown = False
    , lastPosition = { left = 0, top = 0 }
    , navKey = navKey
    , now = Time.millisToPosix 0
    , mode = Exploring
    , position = { left = 0, top = 0 }
    , startTime = Time.millisToPosix 0
    , state = Unstarted
    , target = { left = 0, top = 0 }
    }
        |> fx []


type alias Settings =
    {}


type GameState
    = Running
    | Unstarted


type GameMode
    = Exploring
    | Chatting (List Character)


type alias MouseCoord =
    { left : Float
    , top : Float
    }


type Fact
    = AnimationFrameElapsed Posix
    | DialogInitiated Character
    | GameStarted Posix
    | LinkClicked UrlRequest
    | MouseHeldDown MouseCoord
    | MouseMovedWhileHeldDown MouseCoord
    | MouseReleased MouseCoord
    | UrlChanged Url


type Msg
    = Fact Fact
    | Intent Intent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        apply fact =
            case fact of
                AnimationFrameElapsed now ->
                    { model
                        | before = model.now
                        , now = now
                    }
                        |> placeHero
                        |> fx []

                DialogInitiated character ->
                    { model
                        | mode = Chatting [ character ]
                    }
                        |> fx []

                GameStarted startTime ->
                    { model
                        | state = Running
                        , startTime = startTime
                    }
                        |> fx []

                LinkClicked (Browser.Internal url) ->
                    model |> fx []

                LinkClicked (Browser.External url) ->
                    model |> fx []

                MouseHeldDown coords ->
                    { model | isMouseDown = True }
                        |> fx []
                        |> Debug.log ("MouseHeldDown " ++ Debug.toString coords)

                MouseMovedWhileHeldDown coords ->
                    model
                        |> fx []
                        |> Debug.log ("MouseMovedWhileHeldDown " ++ Debug.toString coords)

                MouseReleased coords ->
                    { model
                        | isMouseDown = False
                        , lastPosition = model.position
                        , target = coords
                    }
                        |> fx []
                        |> Debug.log ("MouseReleased " ++ Debug.toString coords)

                UrlChanged url ->
                    model |> fx []

        interpret intent =
            case intent of
                StartGame ->
                    if model.state == Unstarted then
                        model
                            |> fx
                                [ Task.perform (Fact << GameStarted) Time.now
                                ]
                    else
                        model |> fx []

                TalkTo character ->
                    if model.mode == Exploring then
                        model
                            |> fx
                                [ cmd (DialogInitiated character)
                                ]
                    else
                        model |> fx []
    in
    case msg of
        Fact fact ->
            apply fact

        Intent intent ->
            interpret intent


placeHero : Model -> Model
placeHero ({ position, target } as model) =
    if position.left == target.left && position.top == target.top then
        model
    else
        let
            step =
                5.0

            epsilon =
                0.5

            dir =
                { left = target.left - position.left
                , top = target.top - position.top
                }

            len =
                sqrt (dir.left * dir.left + dir.top * dir.top)

            normalized =
                { left = dir.left / len
                , top = dir.top / len
                }

            newPos =
                { left = (floor >> toFloat) (position.left + (step * normalized.left))
                , top = (floor >> toFloat) (position.top + (step * normalized.top))
                }
        in
        { model | position = newPos }



-- View


view : Model -> Document Intent
view model =
    case model.state of
        Running ->
            case model.mode of
                Exploring ->
                    { title = "Exploring - Game"
                    , body =
                        [ Ui.someScene
                        , Ui.mark "(X)" model.target
                        , Ui.mark "o" model.position
                        ]
                    }

                Chatting participants ->
                    { title = "Chatting - Game"
                    , body =
                        [ Ui.someDialog
                        ]
                    }

        Unstarted ->
            { title = "Main Menu"
            , body =
                [ Ui.mainMenu
                ]
            }



-- Helpers


fx : List (Cmd msg) -> model -> ( model, Cmd msg )
fx cmds model =
    ( model, Cmd.batch cmds )


cmd : Fact -> Cmd Msg
cmd fact =
    Task.perform Fact (Task.succeed fact)
