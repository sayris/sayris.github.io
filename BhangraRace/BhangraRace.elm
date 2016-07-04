module BhangraRace where

import Signal
import Window
import Color
import Keyboard
import Text exposing (defaultStyle)
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input as I
import Time


type RacerState = Stall | Left | Right | Win | Lose
type alias OppStat = (String, Float)

type Update = KeyStroke Int | NewTime Time.Time | Next Int | CountdownTimer Time.Time | OppChoice OppStat
type GameState = Dialog Int | Countdown (Int, OppStat) | Game OppStat

type alias PlayerState = (Float, Int, RacerState)
type alias State = (GameState, PlayerState, PlayerState)


------------------- Global Variables -------------------
scl = 4
img_w = 50 * scl
img_h = 60 * scl

board_w = 1200 

start = (-1 * ((board_w/2) - (img_w/2))) + 20
end = -1 * start

total_dist = abs (end - start)
third = (total_dist / 3) + start
two_third = ((total_dist * 3) / 4) + start

incr_amount = 20

num_prompts = List.length (dialog_prompts (3, 3))
num_countdwn = List.length countdown_prompts


dialog_prompts : (Int, Int) -> List E.Element 
dialog_prompts dims =
    [title_dialog dims
    , instruction_dialog dims
    , pick_opponent dims
    ]

dialogColor : Color.Color 
dialogColor = Color.rgba 0 0 0 0.03

countdown_prompts : List Text.Text 
countdown_prompts = 
    let ha = { defaultStyle | typeface = ["Calibri", "Arial"] } in 
    [ Text.style {ha | height = Just 100 } (Text.fromString "Bhangra")
    , Text.style {ha | height = Just 150 } (Text.fromString "Bhangra")
    , Text.style {ha | height = Just 200 } (Text.fromString "Bhangra")
    , Text.style {ha | height = Just 200 } (Text.fromString "Brrrrraaaaaah!")
    ]

initState = (Dialog 0, (start, 0, Stall), (start, 0, Stall))

------------------- Helper Functions -------------------
extractIndex : Int -> List a -> a
extractIndex i xs =
    case (i, xs) of
        (0, x::xs') -> x
        (_, x::xs') -> extractIndex (i - 1) xs'
        (_, _) -> Debug.crash("extractIndex: something happened that shouldnt've") 


textStyle : String -> Text.Text 
textStyle msg =
    Text.style { defaultStyle | typeface = ["Calibri", "Arial"], height = Just 35 } (Text.fromString msg) 

textStyle2 : String -> Text.Text 
textStyle2 msg =
    Text.style { defaultStyle | typeface = ["Calibri", "Arial"], height = Just 300 } (Text.fromString msg) 


------------------- Signal Functions / Mailboxes / Buttons -------------------

nextBtn : Signal.Mailbox Int
nextBtn = Signal.mailbox 0

dialogButton : E.Element
dialogButton = 
    I.customButton (Signal.message nextBtn.address 1)
        (E.image 100 50 "button_pic/btn.png")
        (E.image 100 50 "button_pic/btn_highlight.png")
        (E.image 100 50 "button_pic/btn.png")

sm_dialogButton : E.Element
sm_dialogButton = 
    I.customButton (Signal.message nextBtn.address 1)
        (E.image 60 30 "button_pic/btn.png")
        (E.image 60 30 "button_pic/btn_highlight.png")
        (E.image 60 30 "button_pic/btn.png")
 
resetButton : E.Element
resetButton =
    let w = round (5.5 * 25) in
    let h = round (2.5 * 25) in 
    I.customButton (Signal.message nextBtn.address -5627) 
        (E.image w h "button_pic/again.png")
        (E.image w h "button_pic/again_highlight.png")
        (E.image w h "button_pic/again.png")

-- (opponent name, "speed"/difficultly level)
opponentBtn : Signal.Mailbox OppStat
opponentBtn = Signal.mailbox ("blah", 0)

richard : E.Element
richard =
    I.customButton (Signal.message opponentBtn.address ("richard", 0.5))
        (E.image 150 150 "button_pic/button_richard.png")
        (E.image 150 150 "button_pic/highlight_richard.png")
        (E.image 150 150 "button_pic/button_richard.png")

christina : E.Element
christina =
    I.customButton (Signal.message opponentBtn.address ("christina", 0.8)) 
        (E.image 150 150 "button_pic/button_christina.png")
        (E.image 150 150 "button_pic/highlight_christina.png")
        (E.image 150 150 "button_pic/button_christina.png")

mai : E.Element
mai =
    I.customButton (Signal.message opponentBtn.address ("mai", 0.7))
        (E.image 150 150 "button_pic/button_mai.png")
        (E.image 150 150 "button_pic/highlight_mai.png")
        (E.image 150 150 "button_pic/button_mai.png")


sayri : E.Element
sayri =
    I.customButton (Signal.message opponentBtn.address ("sayri", 1))
        (E.image 150 150 "button_pic/button_sayri.png")
        (E.image 150 150 "button_pic/highlight_sayri.png")
        (E.image 150 150 "button_pic/button_sayri.png")

game_sig : Signal Update
game_sig =
    let map_key {x, y} = KeyStroke x in
    Signal.mergeMany
    [ Signal.map NewTime (Time.fps 11)
    , Signal.map CountdownTimer (Time.every Time.second)
    , Signal.map map_key Keyboard.arrows
    , Signal.map Next nextBtn.signal
    , Signal.map OppChoice opponentBtn.signal
    ]



------------------- Game State Updates -------------------

-- updating and indivual Bhangra Racer's state on the board
up_player_state : Int -> PlayerState -> Float -> PlayerState 
up_player_state x (x_val, prev_key, state) speed =
    if x_val > end then
        if state /= Lose then
            (x_val, prev_key, Win)
        else 
            (x_val, prev_key, Lose)
    else if x /= prev_key && state /= Win then
        if x == -1 then
            (x_val + (incr_amount * speed), x, Left) -- left key
        else if x == 1 then
            (x_val + (incr_amount * speed), x, Right) -- right key
        else (x_val, prev_key, state)
    else (x_val, prev_key, state)

-- updating the entire state of the game 
-- (delegates updating each of the individual racers to 
-- helper function... distinguishes between the "computer" player
-- and the "actual" player).
-- Determines if there is a winner and if so
-- also designates the loser
game_update : Update -> State -> Float -> State
game_update up (gs, p1, p2) opp_speed =
    let (x1, k1, s1) = p1 in 
    let (x2, k2, s2) = p2 in 
    case up of
        KeyStroke k -> if s1 == Win && s2 /= Win then 
                            (gs, up_player_state k p1 1, (x2, k2, Lose))
                        else if s2 == Win && s1 /= Win then
                            (gs, up_player_state k (x1, k1, Lose) 1, p2)
                        else (gs, up_player_state k p1 1, p2)
        NewTime t -> if k2 == 0 then (gs, p1, up_player_state 1 p2 opp_speed)
                     else 
                        if s1 == Win && s2 /= Win then 
                            (gs, p1, up_player_state (k2 * -1) (x2, k2, Lose) opp_speed)
                        else if s2 == Win && s1 /= Win then
                            (gs, (x1, k1, Lose), up_player_state (k2 * -1) p2 opp_speed)
                        else (gs, p1, up_player_state (k2 * -1) p2 opp_speed)
        Next i -> if i == -5627 then (Dialog 2, (start, 0, Stall), (start, 0, Stall))
                else Debug.crash "game_update shouldn't have been called"
        _ -> Debug.crash "game_update shoudln't have been called"
        --CountdownTimer i -> Debug.crash "game_update shouldn't have been called"
        --OppChoice bleh -> Debug.crash "game_update shouldn't have been called"


-- general function for updating the state
upstate : Update -> State -> State 
upstate up st =
    let (gs, p1, p2) = st in 
    case (gs, up) of 
        (Dialog d, Next i) -> let next = (d + i) % num_prompts in 
                            if d > num_prompts then Debug.crash "error in calculations"
                            else if next == 0 then (Countdown (0, ("blah", 0)), p1, p2)
                            else (Dialog next, p1, p2)
        (Dialog d, OppChoice choice) -> (Countdown (0, choice), p1, p2)
        (Dialog d, _) -> st
        (Countdown (d, choice), CountdownTimer i) -> let next = d + 1 in 
                                        if next >= num_countdwn then (Game choice, p1, p2)
                                        else (Countdown (next, choice), p1, p2)
        (Countdown (d, choice), _) -> st  
        (Game (opp_name, opp_speed), CountdownTimer i) -> st
        (Game (opp_name, opp_speed), _) -> game_update up st opp_speed

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

opponent : PlayerState -> GameState -> C.Form
opponent (x, _, st) gs =
    let ll = 8 in 
    let body = "opponents/faceless_blue_body.png" in
    let face x name = if x < third then ("opponents/" ++ name ++ "_1.png") 
                 else if x < two_third then ("opponents/" ++ name ++ "_2.png")
                 else ("opponents/" ++ name ++ "_3.png") in
    let guy left right name = 
        C.group 
            [C.toForm (E.image img_w img_h "l_leg.png")
                |> C.move (x, left), 
             C.toForm (E.image img_w img_h "r_leg.png")
                |> C.move (x, right),
             C.toForm (E.image img_w img_h body)
                |> C.moveX x,
             C.toForm (E.image img_w img_h (face x name))
                |> C.moveX x
            ] in
    case (st, gs) of
        (Left, Game (nm, spd))  -> guy ll 0 nm
        (Right, Game (nm, spd)) -> guy 0 ll nm
        (_, Game (nm, spd))     -> guy 0 0 nm
        (_, Countdown (_, (nm, spd))) -> guy 0 0 nm
        (_, _)                  -> guy 0 0 "head"


just_racers : State -> Int -> E.Element
just_racers (gs, p1, p2) h =
    let (x1, k1, s1) = p1 in 
    let (x2, k2, s2) = p2 in 
    C.collage board_w h [C.moveY 130 (opponent p2 gs), C.moveY -100 (player p1)]
        |> E.color Color.white

outcome_racers : (Int, Int) -> State -> E.Element
outcome_racers (w, h) (gs, (_, _, s1), (_, _, s2)) =
    let ww = 7 * 85 in 
    let hh = round (4.5 * 85) in 
    if s1 == Win then 
        E.flow E.down 
            [ E.spacer 1 120
            , (E.image ww hh "win.png") 
                |> E.container w 385 E.middle
            , E.container w 60 E.middle resetButton
            ]
    else if s2 == Win then 
        E.flow E.down 
            [ E.spacer 1 110
            , (E.image ww hh "lose.png") 
                |> E.container w 395 E.middle
            , E.container w 80 E.middle resetButton
            ]
    else E.spacer 0 0


title_dialog : (Int, Int) -> E.Element 
title_dialog (w, h) = 
    E.flow E.inward
        [E.flow E.down 
                    [E.image 400 400 "button_pic/title.png"
                    , E.spacer 1 40
                    , E.flow E.right 
                        [E.spacer 140 100, dialogButton]]
                |> E.container w h E.middle
        , C.collage 500 600 [C.filled dialogColor (C.rect 450 550) |> C.moveY 20]
            |> E.container w h E.middle
        ]

instruction_dialog : (Int, Int) -> E.Element
instruction_dialog (w, h) =
    let msg =  "Move your player by hitting\n the L and R arrow keys" in
    E.flow E.down 
        [E.spacer 1 30
        , E.centered (textStyle msg) 
            |> E.size 630 80 
        , E.container 630 150 E.middle (E.image 240 150 "button_pic/arrow.gif")
        , E.container 550 50 E.topRight dialogButton
        ]
    |> E.size 630 320 
    |> E.color dialogColor
    |> E.container w h E.middle

pick_opponent : (Int, Int) -> E.Element
pick_opponent (w, h) = 
    let msg = "Pick your opponent:" in 
    E.flow E.down
        [E.spacer 1 30
        , E.centered (textStyle msg)
            |> E.size 600 70
        , E.flow E.right [richard, mai, christina, sayri]
            |> E.container 630 150 E.middle
        ]
    |> E.size 630 300
    |> E.color dialogColor
    |> E.container w h E.middle


dialog : Int -> (Int, Int) -> E.Element
dialog num dim = 
    extractIndex num (dialog_prompts dim)

countdown_screen : Int -> (Int, Int) -> E.Element
countdown_screen num (w, h) =
    let msg = extractIndex num countdown_prompts in 
    E.container w h E.middle (E.centered msg)

display_elements : (Int, Int) -> State -> List E.Element 
display_elements dims st = 
    let (w, h) = dims in 
    let (gs, p1, p2) = st in
    let centered_racers = E.container w h E.middle (just_racers st h) in
    let outcome = outcome_racers dims st in 
    case gs of 
        Dialog i -> [centered_racers, dialog i dims]
        Countdown (i, choice) -> [centered_racers, countdown_screen i dims]
        Game nm -> [centered_racers, outcome]

view : (Int, Int) -> State -> E.Element
view dims st =
    let elems = display_elements dims st in
    E.flow E.outward elems |> E.color (Color.rgba 242 212 121 1)

------------------- Main -------------------

main : Signal E.Element
main =
    Signal.map2 view Window.dimensions 
        (Signal.foldp upstate initState game_sig)

