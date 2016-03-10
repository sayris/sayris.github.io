module Main where 

import BlendokuTiles as BT 
import GameBoard as GB
import Mouse 
import Svg as S
import Svg.Attributes as SA
import Html exposing (..)
import Html.Attributes as HA
import Window as W
import Random
import GameUI as GUI
import Time
import Array
import Maybe
import GlobalVars as GV 
import Spacing as Sp
import Levels as L 

type GameState = Playing | Win | UnSeeded
type alias GameLevel = Maybe.Maybe Int 

type alias State = (List BT.Tile, GB.GameBoard, GameState, GameLevel)

type alias MoveEvent = (Int, BT.TileEvent)
type Event = Mv MoveEvent | Btn GUI.UISignal | Wnd (Int, Int)

type alias Snp = ((Int, Int), GB.GameBoard, BT.TileStatus)
type SnapOrSwap = Snap Snp | Swap (Int, GB.GameBoard) 

tileMailbox = BT.tileMailbox
buttonMailbox = GUI.uiMailbox

extractState : State -> (List BT.Tile, GB.GameBoard, GameState, GameLevel) 
extractState (tiles, gb, ts, l) =
    (tiles, gb, ts, l) 

extractLevel : State -> Maybe.Maybe Int 
extractLevel (tiles, gb, ts, l) = l

mousePosSignal : Signal Event
mousePosSignal =
    Signal.map2 
        (\(int, sig) mousePos -> 
            case sig of 
                BT.Selected -> Mv (int, BT.Drag mousePos)
                _        -> Mv (int, sig))
        tileMailbox.signal
        Mouse.position


buttonSignals : Signal Event
buttonSignals =
    Signal.map3 
        (\sig time dims->
            case sig of 
                GUI.NeedTime -> Btn (GUI.NewGame (time, dims)) 
                _            -> Btn sig)
        buttonMailbox.signal 
        (Signal.sampleOn buttonMailbox.signal (Time.every Time.millisecond))
        (Signal.sampleOn buttonMailbox.signal (W.dimensions))

gameSignals : Signal Event
gameSignals =
    Signal.merge mousePosSignal 
        (Signal.merge buttonSignals 
            (Signal.map Wnd W.dimensions))



--snap : (Int, Int) -> Int -> GameBoard -> SnapOrSwap
--snap (x, y) tileIndex bb =
--    case bb of 
--        [] -> Snap ((x, y), [], NotPlaced) 
--        b::bs -> 
--            let (x', y', status) = extractBSP b in 
--            let adjX = x' GV.tileSideLen//3) in
--            let adjY = y' GV.tileSideLen//3) in
--            let deltaX = toFloat ((x - adjX)^2) in
--            let deltaY = toFloat ((y - adjY)^2) in 
--            let dist = sqrt (deltaX + deltaY) in

--            if dist < 40 then 
--                case b.status of
--                    Empty ->  
--                    Snap ( (adjX, adjY), 
--                           {b | status = Occupied tileIndex}::bs,
--                           Placed b.index)
--                    Occupied ti -> Swap (ti, {b | status = Occupied tileIndex}::bs) 
--            else 
--                let result = snap (x, y) tileIndex bs in
--                case result of 
--                    Snap (pos, board, status) -> Snap (pos, b::board, status)
--                    _ -> result 


{- given that i2 is index of tile currently occupying the board spot-}
{- swapTiles : Int -> Int -> List Tile -> List Tile 
swapTiles i1 i2 tiles =
    let t1 = getTile i1 tiles in
    let t2 = getTile i2 tiles in 
    let t2Status = t2.status in
    let t2Pos = t2.pos in 
    let up1 = case t1.status of 
            NotPlaced -> updateTile {t2 | status = NotPlaced, pos = t2.originPos} tiles
            Placed a -> updateTile {t2 | status = Placed a, pos = t1.pos} tiles in
    updateTile {t1 | status = t2Status, pos = t2.pos} up1 -}

snap : (Int, Int) -> Int -> GB.GameBoard -> Snp
snap (x, y) tileIndex bb =
    case bb of 
        [] -> ((x, y), [], BT.NotPlaced) 
        b::bs -> 
            let (x', y', status) = GB.extractBSP b in 
            let adjX = x' - (GV.tileSideLen//3) in
            let adjY = y' - (GV.tileSideLen//3) in
            let deltaX = toFloat ((x - adjX)^2) in
            let deltaY = toFloat ((y - adjY)^2) in 
            let dist = sqrt (deltaX + deltaY) in

            let ifUpdate = ((adjX, adjY), {b | status = GB.Occupied tileIndex}::bs, BT.Placed b.index) in 
            case (b.status, dist < 40) of
                (GB.Occupied i, True) -> ifUpdate
                    --if i == tileIndex then ifUpdate else 
                    --((adjX, adjY), b::bs, BT.NotPlaced)
                (GB.Empty, True) -> ifUpdate
                _ -> let (pos, board, status) = snap (x, y) tileIndex bs in
                    (pos, b::board, status)
            --    (_, True) -> ((adjX, adjY), {b | status = GB.Occupied tileIndex}::bs, BT.Placed b.index)
            --if dist < 40 then 
            --    ((adjX, adjY), {b | status = GB.Occupied tileIndex}::bs, BT.Placed b.index) 
            --else 
            --    let (pos, board, status) = snap (x, y) tileIndex bs in
            --    (pos, b::board, status)



checkGame : Int -> GB.GameBoard -> GameState
checkGame prev g =
    case g of 
        [] -> Win 
        g'::gg -> 
            let (x, y, st) = GB.extractBSP g' in
            case st of 
                GB.Empty -> Playing 
                GB.Occupied d -> 
                    if d /= prev + 1 then Playing 
                    else checkGame d gg 
                GB.Set d -> checkGame d gg 

checkWin : State -> State 
checkWin (tiles, gameboard, st, l) =
    case tiles of 
        [] -> (tiles, gameboard, st, l)
        _  -> let w = checkGame -1 gameboard in
              (tiles, gameboard, w, l)

moveTile : MoveEvent -> State -> State 
moveTile (i, ts) (s, b, st, l) =
    case s of
        [] -> ([], b, st, l)
        s'::ss -> 
            case ((s'.index == i), ts, s'.status) of 
                (True, _, BT.Set puy) -> 
                    let (tiles, board, st, l) = moveTile (i, ts) (ss, b, st, l) in
                    (s'::tiles, board, st, l)
                (True, BT.Drag (x, y), _) ->
                    let x' = x - (GV.tileSideLen//2) in
                    let y' = y - (GV.tileSideLen//2) in
                    let ((snapx, snapy), newBoard, newSt) = snap (x', y') i b in
                    ({s' | pos = (snapx, snapy), status = newSt} :: ss, newBoard, st, l)
                (True, BT.Rest, BT.NotPlaced) -> ({s' | pos = s'.originPos}::ss, GB.cleanBoard s'.index b, st, l)
                (True, _, _) -> (s'::ss, b, st, l)
                _ -> 
                    let (tiles, board, st, l) = moveTile (i, ts) (ss, b, st, l) in
                    (s'::tiles, board, st, l)
            --if s'.index == i then
            --    case ts of
            --        BT.Drag (x, y) -> 
            --            let x' = x - (GV.tileSideLen//2) in
            --            let y' = y - (GV.tileSideLen//2) in
            --            let ((snapx, snapy), newBoard, newSt) = snap (x', y') i b in
            --            ({s' | pos = (snapx, snapy), status = newSt} :: ss, newBoard, st, l)
            --        BT.Rest -> 
            --            if s'.status == BT.NotPlaced then 
            --                ({s' | pos = s'.originPos}::ss, GB.cleanBoard s'.index b, st, l)
            --            else 
            --                case s'
            --                (s'::ss, b, st, l)
            --        BT.Selected -> (s'::ss, b, st, l)
            --else let (tiles, board, st, l) = moveTile (i, ts) (ss, b, st, l) in 
            --    (s'::tiles, board, st, l)

resetBoard : List BT.Tile -> GB.GameBoard -> (List BT.Tile, GB.GameBoard)
resetBoard tiles gb = 
    let setTiles = List.filterMap BT.isSet2 tiles in 
    let setTiles' = List.filter BT.isSet tiles in 
    let setBSP = List.map (\(bI, tI) -> GB.getBSP bI gb) setTiles in 
    let resetB = GB.resetBoard gb in
    let resetT = List.map (\t -> {t | pos = t.originPos, status = BT.NotPlaced}) tiles in 
    let resetT' = List.foldl (\a b -> BT.updateTiles a b) resetT setTiles' in 
    let resetB' = List.foldl (\a b -> GB.updateBSP a b) resetB setBSP in 
    (resetT', resetB')  


buttonUpdate : GUI.UISignal -> State -> State 
buttonUpdate sig (tile, gb, status, l) =
    let sigStr = toString sig in
    -- we know Reset cannot be hit if the game has not been seeded yet 
    -- therefore there will *always* be a level 
    case sig of 
        GUI.NewGame f -> generateGameLevels f
        GUI.Reset -> 
            let (tiles', board') = resetBoard tile gb in 
            (tiles', board', Playing, l)
        _ -> Debug.log sigStr
            (tile, gb, status, l) 

update : Event -> State -> State 
update e st =
    let (tiles, board, gstate, l) = extractState st in
    let lvl = Maybe.withDefault 0 l in    
    case gstate of 
        Win -> case e of 
                Btn ss     -> buttonUpdate ss st 
                Wnd dimens -> let newB = respaceBoard dimens lvl board in 
                              let newT = respaceTiles dimens newB tiles in 
                              (newT, newB, gstate, l)
                _          ->  st
        Playing -> case e of 
                Mv (i, ts) -> let newState = moveTile (i, ts) st in 
                            checkWin newState
                Btn ss     -> buttonUpdate ss st 
                Wnd dimens -> let newB = respaceBoard dimens lvl board in 
                              let newT = respaceTiles dimens newB tiles in 
                              (newT, newB, gstate, l)
        UnSeeded -> case e of 
                    Btn ss -> buttonUpdate ss st 
                    _      -> st 

unwrapJust : Maybe.Maybe a -> a 
unwrapJust a =
    case a of 
        Just b -> b
        Nothing -> Debug.crash "unwrapJust: got Nothing"

rtHelper : List BT.Tile -> Array.Array (Int, Int) -> Array.Array GB.BoardSnapPos -> List BT.Tile 
rtHelper tiles coords gb = 
    case tiles of 
        [] -> []
        t :: tt -> 
            case t.status of 
                BT.NotPlaced -> let newOrigin = unwrapJust (Array.get (round t.randI) coords) in 
                {t | pos = newOrigin, originPos = newOrigin} :: (rtHelper tt coords gb)

                BT.Placed a -> let newOrigin = unwrapJust (Array.get (round t.randI) coords) in
                let g = unwrapJust (Array.get a gb) in 
                let newPos = GV.getCenterPos g.pos in 
                {t | pos = newPos, originPos = newOrigin} :: (rtHelper tt coords gb)

                BT.Set a -> let g = unwrapJust (Array.get a gb) in 
                let center = GV.getCenterPos g.pos in 
                {t | pos = center, originPos = center} :: (rtHelper tt coords gb)

respaceBoard : (Int, Int) -> Int -> GB.GameBoard -> GB.GameBoard 
respaceBoard dimens lvl board = 
    let spaceB = (L.getBoardPos lvl) dimens in 
    List.map2 (\p b -> {b| pos = p}) spaceB board

respaceTiles : (Int, Int) -> GB.GameBoard -> List BT.Tile -> List BT.Tile 
respaceTiles dimens gb t = 
    let numTiles = List.length t in 
    let newCoords = Array.fromList (Sp.newSpaceTiles dimens numTiles GV.colWidth) in 
    let gb'  = Array.fromList gb in 
    rtHelper t newCoords gb' 

drawGameBoard : (Int, Int) -> State -> Html
drawGameBoard (w, h) (tiles, board, st, l) =
    div 
    []
    [
        S.svg
            [ SA.width  (toString w)
            , SA.height (toString 500)
            ]
            (List.append 
                        (List.map GB.boardImage board)
                        (List.map (\b -> (BT.tileImage (w, h) b)) tiles))
    ]

view : State -> (Int, Int) -> Html
view st windowDim =
    let (tiles, board, status, l) = extractState st in
    case status of 
        UnSeeded -> div [] [GUI.newGameButton]
        Playing -> div []
            [drawGameBoard windowDim st
            , GUI.buttonGroup]
        Win -> div [] 
            [div [HA.class "win-wrapper"] [div [HA.class "winner"] [text "You Win!"]]
            , drawGameBoard windowDim st
            , GUI.buttonGroup]
    
initState : State 
initState =
    ([], [], UnSeeded, Maybe.Nothing)

setTiles : List Int -> List BT.Tile -> GB.GameBoard -> (List BT.Tile, GB.GameBoard)
setTiles ss ts gbs = 
    let tiles = List.map (\i -> BT.getTile i ts) ss in 
    let bsp = List.map (\i -> GB.getBSP i gbs) ss in 
    let bPos = List.map (\b -> (GV.getCenterPos b.pos, b.index)) bsp in 
    let tiles' = 
        List.map2 (\t (pos, i) -> {t | pos = pos, originPos = pos, status = BT.Set i}) tiles bPos in 
    let bsp' = List.map2 (\b t-> {b | status = GB.Set t.index}) bsp tiles' in 
    let tt = List.foldl (\a b -> BT.updateTiles a b) ts tiles' in 
    let bb = List.foldl (\a b -> GB.updateBSP a b) gbs bsp' in 
    (tt, bb)

genRLevel : Random.Seed -> (Int, Random.Seed)
genRLevel s = 
    let gen = Random.int 0 189 in 
    Random.generate gen s 

--generateGame : (Float, (Int, Int)) -> State 
--generateGame (f, dimens) =
--    let tiles = BT.genTiles (Random.initialSeed (round f)) dimens in 
--    let boardState = GB.genInitGameBoard (List.length tiles) dimens in
--    let (t, b) = setTiles [0, 6] tiles boardState in
--    (t, b, Playing)

generateGameLevels : (Float, (Int, Int)) -> State
generateGameLevels (f, (x, y)) =
    let (lvl, s) = genRLevel (Random.initialSeed (round f)) in 
    let l' = ((lvl%6) + 1) in 
    let (bf, tl, ts) = L.getLevel l' in 
    let tiles = tl (x, 550) s in 
    let board = bf (x, 550) in 
    let (t', b') = setTiles ts tiles board in 
    (t', b', Playing, Maybe.Just l')

main : Signal Html
main = 
    Signal.map2 view 
        (Signal.foldp update initState gameSignals)
        W.dimensions