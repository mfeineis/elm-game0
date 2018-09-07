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
import Element
import Html
import Json.Decode as Decode exposing (Decoder, Value)
import Task
import Time exposing (Posix)
import UI exposing (Intent(..))
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = TechnicalFact << UrlChanged
        , onUrlRequest = TechnicalFact << LinkClicked
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
        [ onAnimationFrame (TechnicalFact << AnimationFrameElapsed)
        , onMouseDown (decodeMouseCoord (TechnicalFact << MouseHeldDown))
        , if isMouseDown then
            onMouseMove (decodeMouseCoord (TechnicalFact << MouseMovedWhileHeldDown))
          else
            Sub.none
        , onMouseUp (decodeMouseCoord (TechnicalFact << MouseReleased))
        ]


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url navKey =
    { before = Time.millisToPosix 0
    , isMouseDown = False
    , lastPosition = { left = 0, top = 0 }
    , navKey = navKey
    , now = Time.millisToPosix 0
    , position = { left = 0, top = 0 }
    , startTime = Time.millisToPosix 0
    , state = Unstarted
    , target = { left = 0, top = 0 }
    }
        |> fx []


type alias Model =
    { before : Posix
    , isMouseDown : Bool
    , lastPosition : MouseCoord
    , navKey : Navigation.Key
    , now : Posix
    , position : MouseCoord
    , startTime : Posix
    , state : GameState
    , target : MouseCoord
    }


type alias Settings =
    {}


type GameState
    = Running
    | Unstarted


type alias MouseCoord =
    { left : Float
    , top : Float
    }


type TechnicalFact
    = AnimationFrameElapsed Posix
    | LinkClicked UrlRequest
    | MouseHeldDown MouseCoord
    | MouseMovedWhileHeldDown MouseCoord
    | MouseReleased MouseCoord
    | UrlChanged Url


type DomainFact
   = GameStarted Posix


type Msg
    = DomainFact DomainFact
    | Intent Intent
    | TechnicalFact TechnicalFact


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        applyTechnical fact =
            case fact of
                AnimationFrameElapsed now ->
                    { model
                        | before = model.now
                        , now = now
                    }
                        |> placeHero
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
                                [ Task.perform (DomainFact << GameStarted) Time.now
                                ]
                    else
                        model |> fx []

        applyDomain fact =
            case fact of
                GameStarted startTime ->
                    { model
                        | state = Running
                        , startTime = startTime
                    }
                        |> fx []
    in
    case msg of
        DomainFact fact ->
            applyDomain fact

        Intent intent ->
            interpret intent

        TechnicalFact fact ->
            applyTechnical fact


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
            { title = "Game"
            , body =
                [ UI.mark "(X)" model.target
                , UI.mark "o" model.position
                ]
            }

        Unstarted ->
            { title = "Main Menu"
            , body =
                [ UI.mainMenu
                ]
            }



-- Helpers


fx : List (Cmd msg) -> model -> ( model, Cmd msg )
fx cmds model =
    ( model, Cmd.batch cmds )
