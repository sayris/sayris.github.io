module GameBoard where 

import Svg as S
import Svg.Attributes as SA
import BlendokuTiles as BT
import Html exposing (..)
import GlobalVars as GV
import Spacing as S 

type BoardStatus = Empty | Occupied Int | Set Int 
type alias BoardSnapPos = {index : Int, pos : (Int, Int), status : BoardStatus}
type alias GameBoard = List BoardSnapPos

extractBSP {pos,status} =
    case pos of
        (x, y) -> (x, y, status)

genInitGameBoard : Int -> (Int, Int) -> GameBoard
genInitGameBoard num (w, h)= 
    let adjP = S.spacingBoard (w, h) num in 
    List.map2 (\b x-> {index=b, pos=x, status = Empty})
        [0..(num - 1)]
        adjP

getBSP : Int -> GameBoard -> BoardSnapPos 
getBSP index bsp =
    case bsp of
        [] -> Debug.crash "getBSP: looking for bsp that doesn't exist!"
        t::tt -> if t.index == index then t else getBSP index tt 


updateBSP : BoardSnapPos -> GameBoard -> GameBoard 
updateBSP t bsp =
    case bsp of
        []    -> Debug.crash "updateBSP: bsp does not exists!"
        t'::tt -> 
            if t.index == t'.index then t::tt 
            else t'::(updateBSP t tt)
boardImage : BoardSnapPos -> S.Svg
boardImage b =
    let msg = (toString b.index) ++ " " ++ (toString b.status) in
    let posX = toString (fst b.pos) in 
    let posY = toString (snd b.pos) in 
    let color = "rgb(105, 105, 105)" in 
    let sideLen = GV.tileSideLen//3 in 
    S.rect
        [ SA.x posX
        , SA.y posY
        , SA.rx "2"
        , SA.ry "2"
        , SA.width (toString sideLen)
        , SA.height (toString sideLen)
        , SA.fill color
        ]
        [text msg]

cleanBoard : Int -> GameBoard -> GameBoard 
cleanBoard n gb =
    case gb of
        [] -> []
        g::gg -> 
            case g.status of 
                Occupied d -> 
                    if n == d then {g | status = Empty} :: gg
                    else g::(cleanBoard n gg)
                _ -> g::(cleanBoard n gg)

resetBoard : GameBoard -> GameBoard
resetBoard gb =
    case gb of 
        [] -> []
        g :: gg -> {g | status = Empty} :: (resetBoard gg)