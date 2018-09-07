module Game.Ui
    exposing
        ( Intent(..)
        , mainMenu
        , mark
        , someDialog
        , someScene
        )

import Element exposing (Element, column, el, layout, link, row, text)
import Element.Input as Input exposing (button)
import Game.Character exposing (Character(..))
import Html exposing (Html)
import Html.Attributes as Attr


type Intent
    = StartGame
    | TalkTo Character


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


someScene : Html Intent
someScene =
    layout []
        (column []
            [ button []
                { label = el [] (text "Talk to some tree")
                , onPress = Just (TalkTo SomeTree)
                }
            ]
        )


someDialog : Html Intent
someDialog =
    Html.div []
        [ Html.text "Hello Tree!"
        ]


mark : String -> { a | left : Float, top : Float } -> Html intent
mark txt { left, top } =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "left" (String.fromFloat left ++ "px")
        , Attr.style "top" (String.fromFloat top ++ "px")
        ]
        [ Html.text txt
        ]
