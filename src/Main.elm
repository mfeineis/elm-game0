module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation
import Html exposing (Attribute, Html, div, span)
import Html.Attributes as Attr exposing (class)
import UI exposing (Intent(..))
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url navKey =
    { game = Unstarted
    , navKey = navKey
    }
        |> fx []


type alias Model =
    { game : Game
    , navKey : Navigation.Key
    }


type alias Settings =
    {}


type Game
    = Running
    | Unstarted


type Fact
    = GameStarted
    | LinkClicked UrlRequest
    | UrlChanged Url


type Msg
    = Fact Fact
    | Intent Intent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        apply fact =
            case fact of
                LinkClicked (Browser.Internal url) ->
                    model |> fx []

                LinkClicked (Browser.External url) ->
                    model |> fx []

                UrlChanged url ->
                    model |> fx []

                -- Domain
                GameStarted ->
                    { model
                        | game = Running
                    }
                        |> fx []

        interpret intent =
            case intent of
                StartGame ->
                    if model.game == Unstarted then
                        apply GameStarted
                    else
                        model |> fx []
    in
    case msg of
        Fact fact ->
            apply fact

        Intent intent ->
            interpret intent



-- View


view : Model -> Document Intent
view { game } =
    case game of
        Running ->
            { title = "Game"
            , body =
                [ Html.div [] [ Html.text "Running..." ]
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
