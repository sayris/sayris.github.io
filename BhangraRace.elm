module BhangraRace where

import Signal
import Window
import Color
import Keyboard
import Text
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input as I
import Time


type RacerState = Stall | Left | Right | Win | Lose
type Update = KeyStroke Int | NewTime Time.Time | Next Int | CountdownTimer Time.Time
type GameState = Dialog Int | Countdown Int | Game 

type alias PlayerState = (Float, Int, RacerState)
type alias State = (GameState, PlayerState, PlayerState)


------------------- Global Variables -------------------
scl = 4
board_w = 1200 
board_h = 500
img_w = 50 * scl
img_h = 60 * scl
start = (-1 * ((board_w/2) - (img_w/2))) + 20
end = -1 * start
total_dist = abs (end - start)
third = (total_dist / 3) + start
two_third = ((total_dist * 3) / 4) + start
incr_amount = 20
num_prompts = List.length dialog_prompts
num_countdwn = List.length countdown_prompts

dialog_prompts : List String 
dialog_prompts =
    [ "Move your player by hitting the L and R keys"
    , "Select your opponent"
    ]

countdown_prompts : List String
countdown_prompts = ["3", "2", "1", "Go!"]

initState = (Dialog 0, (start, 0, Stall), (start, 0, Stall))

------------------- Helper Functions -------------------
extractIndex : Int -> List a -> a
extractIndex i xs =
    case (i, xs) of
        (0, x::xs') -> x
        (_, x::xs') -> extractIndex (i - 1) xs'
        (_, _) -> Debug.crash("extractIndex: something happened that shouldnt've") 


------------------- Signal Functions / Mailboxes / Buttons -------------------

nextBtn : Signal.Mailbox Int
nextBtn = Signal.mailbox 0


dialogButton : E.Element
dialogButton = 
    I.button (Signal.message nextBtn.address 1) "next"

resetButton : E.Element
resetButton =
    I.button (Signal.message nextBtn.address -5627) "Play Again"


game_sig : Signal Update
game_sig =
    let map_key {x, y} = KeyStroke x in
    Signal.mergeMany
    [ Signal.map NewTime (Time.fps 8)
    , Signal.map CountdownTimer (Time.every Time.second)
    , Signal.map map_key Keyboard.arrows
    , Signal.map Next nextBtn.signal
    ]



------------------- Game State Updates -------------------

-- updating and indivual Bhangra Racer's state on the board
up_player_state : Int -> PlayerState -> PlayerState 
up_player_state x (x_val, prev_key, state) =
    if x_val > end then
        if state /= Lose then
            (x_val, prev_key, Win)
        else 
            (x_val, prev_key, Lose)
    else if x /= prev_key && state /= Win then
        if x == -1 then
            (x_val + incr_amount, x, Left) -- left key
        else if x == 1 then
            (x_val + incr_amount, x, Right) -- right key
        else (x_val, prev_key, state)
    else (x_val, prev_key, state)

-- updating the entire state of the game 
-- (delegates updating each of the individual racers to 
-- helper function... distinguishes between the "computer" player
-- and the "actual" player).
-- Determines if there is a winner and if so
-- also designates the loser
game_update : Update -> State -> State
game_update up (gs, p1, p2) =
    let (x1, k1, s1) = p1 in 
    let (x2, k2, s2) = p2 in 
    case up of
        KeyStroke k -> if s1 == Win && s2 /= Win then 
                            (gs, up_player_state k p1, (x2, k2, Lose))
                        else if s2 == Win && s1 /= Win then
                            (gs, up_player_state k (x1, k1, Lose), p2)
                        else (gs, up_player_state k p1, p2)
        NewTime t -> if k2 == 0 then (gs, p1, up_player_state 1 p2)
                     else 
                        if s1 == Win && s2 /= Win then 
                            (gs, p1, up_player_state (k2 * -1) (x2, k2, Lose))
                        else if s2 == Win && s1 /= Win then
                            (gs, (x1, k1, Lose), up_player_state (k2 * -1) p2)
                        else (gs, p1, up_player_state (k2 * -1) p2)
        Next i -> if i == -5627 then initState
                else Debug.crash "game_update shouldn't have been called"
        CountdownTimer i -> Debug.crash "game_update shouldn't have been called"


-- If game is in the Dialog/instruction phase
-- increments which dialog message to show
--dialog_update : Int -> State -> State 
--dialog_update i (gs, p1, p2) = 
--    case gs of 
--        Dialog x -> let next = (x + i)%num_prompts in
--                            if x >= num_prompts then Debug.crash "Error in calculation"
--                            else if next == 0 then (Countdown 0, p1, p2) 
--                            else (Dialog next, p1, p2) 
--        Game -> Debug.crash "I don't think this should happen?"

-- general function for updating the state
upstate : Update -> State -> State 
upstate up st =
    let (gs, p1, p2) = st in 
    case (gs, up) of 
        (Dialog d, Next i) -> let next = (d + i) % num_prompts in 
                            if d > num_prompts then Debug.crash "error in calculations"
                            else if next == 0 then (Countdown 0, p1, p2)
                            else (Dialog next, p1, p2)
        (Dialog d, _) -> st
        (Countdown d, CountdownTimer i) -> let next = d + 1 in 
                                        if next >= num_countdwn then (Game, p1, p2)
                                        else (Countdown next, p1, p2)
        (Countdown d, _) -> st  
        (Game, CountdownTimer i) -> st
        (Game, _) -> game_update up st 

------------------- View Functions -------------------

player : PlayerState -> C.Form
player (x, _, st) = 
    let ll = 8 in 
    let guy left right = 
        C.group 
            [C.toForm (E.image img_w img_h "l_leg.png")
                |> C.move (x, left), 
             C.toForm (E.image img_w img_h "r_leg.png")
                |> C.move (x, right),
             C.toForm (E.image img_w img_h "yellow_body.png")
                |> C.moveX x
            ] in
    case st of
        Left  -> guy ll 0
        Right -> guy 0 ll
        _     -> guy 0 0 

opponent : PlayerState -> String -> C.Form
opponent (x, _, st) name =
    let ll = 8 in 
    let body = "opponents/faceless_blue_body.png" in
    let face x = if x < third then ("opponents/" ++ name ++ "_1.png") 
                 else if x < two_third then ("opponents/" ++ name ++ "_2.png")
                 else ("opponents/" ++ name ++ "_3.png") in
    let guy left right = 
        C.group 
            [C.toForm (E.image img_w img_h "l_leg.png")
                |> C.move (x, left), 
             C.toForm (E.image img_w img_h "r_leg.png")
                |> C.move (x, right),
             C.toForm (E.image img_w img_h body)
                |> C.moveX x,
             C.toForm (E.image img_w img_h (face x))
                |> C.moveX x
            ] in
    case st of
        Left  -> guy ll 0
        Right -> guy 0 ll
        _     -> guy 0 0 

racers : State -> List C.Form
racers (gs, p1, p2) =
    let (x1, k1, s1) = p1 in 
    let (x2, k2, s2) = p2 in 
    let r1 = (player p1) |> C.moveY -50 in 
    let r2 = (opponent p2 "richard") |> C.moveY 130 in 
    let win_bubble = C.text (Text.height 80 (Text.fromString "YOU WIN!")) in 
    let lose_bubble = C.text (Text.height 80 (Text.fromString "YOU LOSE!")) in 
    let tie_bubble = C.text (Text.height 80 (Text.fromString "IT'S A TIE!")) in 
    if s1 == Win then
        if s2 /= Win then [r2, r1, win_bubble]
        else [r2, r1, tie_bubble]
    else if s2 == Win then
        if s1 /= Win then [r2, r1, lose_bubble]
        else [r2, r1, tie_bubble]
    else
       [r2, r1]


race_track : List C.Form -> E.Element
race_track elems =
    (E.color Color.grey (C.collage board_w board_h elems))

dialog : Int -> (Int, Int) -> E.Element
dialog num (w, h)= 
    let msg = extractIndex num dialog_prompts in
    C.collage 300 300
        [ C.text (Text.fromString msg)
        , C.move (70, -70) (C.toForm dialogButton)  
        ]
    |> E.color Color.green
    |> E.container w h E.middle 

countdown_screen : Int -> (Int, Int) -> E.Element
countdown_screen num (w, h) =
    let msg = extractIndex num countdown_prompts in 
    E.show msg |> E.container w h E.middle

display_elements : (Int, Int) -> State -> List E.Element 
display_elements dims st = 
    let (gs, p1, p2) = st in
    case gs of 
        Dialog i -> [race_track (racers st), E.show st, dialog i dims]
        Countdown i -> [race_track (racers st), E.show st, countdown_screen i dims]
        Game -> [race_track (racers st), (E.flow E.down [E.show st, resetButton])]

view : (Int, Int) -> State -> E.Element
view dims st =
    let elems = display_elements dims st in
    E.flow E.outward elems

------------------- Main -------------------

main : Signal E.Element
main =
    Signal.map2 view Window.dimensions 
        (Signal.foldp upstate initState game_sig)

