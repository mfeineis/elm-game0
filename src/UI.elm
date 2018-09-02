module UI
    exposing
        ( Intent(..)
        , mainMenu
        )

import Element exposing (Element, column, el, link, row, text)
import Element.Input as Input exposing (button)
import Html exposing (Html)


type Intent
    = StartGame


mainMenu : Html Intent
mainMenu =
    Element.layout []
        (column []
            [ el [] (text "Main")
            , button []
                { label = el [] (text "Start")
                , onPress = Just StartGame
                }
            ]
        )
