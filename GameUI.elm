module GameUI where 

import Svg as S
import Svg.Attributes as SA
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Signal 

type UISignal = NewGame (Float, (Int, Int)) | NeedTime | Reset


uiMailbox : Signal.Mailbox UISignal 
uiMailbox = Signal.mailbox NeedTime

resetButton : Html 
resetButton = 
    button 
    [ HA.classList
        [ ("button", True)
        , ("inlineB", True)
        ]
    , HE.onClick uiMailbox.address Reset]
    [text "Reset Current Game"]


newGameButton : Html
newGameButton = 
    button 
    [ HA.classList 
        [ ("button", True)
        , ("centerB", True)
        ]
    , HE.onClick uiMailbox.address NeedTime]
    [text "Start New Game"]


newGameButton2 : Html
newGameButton2 = 
    button 
    [ HA.classList 
        [ ("button", True)
        , ("inlineB", True)
        ]
    , HE.onClick uiMailbox.address NeedTime]
    [text "Start New Game"]


buttonGroup : Html 
buttonGroup =
    div [HA.class "btn-group"] [resetButton, newGameButton2]
