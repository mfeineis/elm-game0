module UI
    exposing
        ( Intent(..)
        , mainMenu
        , mark
        )

import Element exposing (Element, column, el, layout, link, row, text)
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Html.Attributes as Attr


type Intent
    = StartGame


mainMenu : Html Intent
mainMenu =
    layout []
        (column []
            [ el [] (text "Main")
            , button []
                { label = el [] (text "Start")
                , onPress = Just StartGame
                }
            ]
        )


mark : String -> { a | left : Float, top : Float } -> Html intent
mark txt { left, top } =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "left" (String.fromFloat left ++ "px")
        , Attr.style "top" (String.fromFloat top ++ "px")
        ]
        [ Html.text txt
        ]
