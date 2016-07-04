module BlendokuTiles where 

import Signal
import Svg as S
import Svg.Attributes as SA
import Html as H
import Svg.Events as SE
import Color as C
import ColorInterpolation as CI
import Random 
import GlobalVars as GV
import Spacing as S 


type TileEvent = Rest | Selected | Drag (Int, Int)
type TileStatus = Placed Int | NotPlaced | Set Int 
type alias Tile = {index : Int, randI : Float, pos : (Int, Int), originPos : (Int, Int), color : C.Color, status: TileStatus}

tileMailbox : Signal.Mailbox (Int, TileEvent) 
tileMailbox = Signal.mailbox (4, Rest)

isSet : Tile -> Bool 
isSet t =
    case t.status of 
        Set a -> True
        _     -> False

isSet2 : Tile -> Maybe (Int, Int)
isSet2 t =
    case t.status of 
        Set a -> Just (a, t.index) 
        _     -> Nothing 

defaultTile : Tile 
defaultTile = 
    {index = -20, randI = -12.43, pos = (0, 0), originPos = (34, 23), color = C.green, status = NotPlaced}

makeTile : Int -> Float -> (Int, Int) -> (Int, Int) -> C.Color -> TileStatus -> Tile 
makeTile i r p op c s =
    {index = i, randI = r, pos = p, originPos = op, color = c, status = s}

tileImage : (Int, Int) -> Tile -> S.Svg
tileImage (w, h) {index, randI, pos, originPos, color, status} =
    let posX = fst pos in
    let posY = snd pos in
    let posX' = if posX > (w - 100) then (w - 100) else posX in
    let posY' = if posY > (h - 100) then (h - 100) else posY in
    let c'  = C.toRgb color in
    let color' = "rgb(" ++ (toString c'.red) ++ "," 
                       ++ (toString c'.green) ++ ","
                       ++ (toString c'.blue) ++ ")" in
    let str = "i: " ++ toString index ++ ", rI: " ++ toString randI ++ ", st: " ++ toString status in
    S.rect
        [ SA.x (toString posX')
        , SA.y (toString posY')
        , SA.rx "4"
        , SA.ry "4"
        , SA.width (toString GV.tileSideLen)
        , SA.height (toString GV.tileSideLen)
        , SA.fill color'
        , SE.onMouseUp (Signal.message tileMailbox.address (index, Rest))
        , SE.onMouseDown (Signal.message tileMailbox.address (index, Selected))
        ]
        [H.text str]

getTile : Int -> List Tile -> Tile 
getTile index tiles =
    case tiles of
        [] -> Debug.crash "getTile: looking for tile that doesn't exist!"
        t::tt -> if t.index == index then t else getTile index tt 


updateTiles : Tile -> List Tile -> List Tile 
updateTiles t tiles =
    case tiles of
        []    -> Debug.crash "updateTile: tile does not exists!"
        t'::tt -> 
            if t.index == t'.index then t::tt 
            else t'::(updateTiles t tt)


genRIndex : Int -> Random.Seed -> (List Float, Random.Seed)
genRIndex n s =
    let floatList n = Random.list n (Random.float 0 5) in 
    Random.generate (floatList n) s 


genTiles2 : Random.Seed -> (Int, Int) -> List C.Color -> List (Int, Int) -> List Tile 
genTiles2 seed (w, h) c pos = 
    let nTile = List.length c in 
    let (randList, s) = genRIndex nTile seed in 
    let tiles = List.map3 
                (\c rI i -> {index = i, randI = rI, pos = (0, 0), originPos = (0, 0), color = c, status = NotPlaced})
                c
                randList 
                [0..(nTile-1)] in
    let tiles1 = List.sortBy .randI tiles in
    let tiles2 = List.map3 
                    (\pos tile i -> {tile | randI = (toFloat i), pos = pos, originPos = pos}) 
                    pos 
                    tiles1
                    [0..(nTile-1)] in 
    tiles2 


genTiles : Random.Seed -> (Int, Int) -> List Tile
genTiles seed (w, h) = 
    let ((c1, c2), rand) = CI.genColor seed in 
    let colorList = CI.interpList GV.numTiles c1 c2 in
    let (randList, s) = genRIndex GV.numTiles seed in 
    let xPos = S.spacingTile (w, h) GV.numTiles in
    let tiles = List.map4 
                (\c rI i pos-> {index = i, randI = rI, pos = pos, originPos = pos, color = c, status = NotPlaced})
                colorList
                randList 
                [0..(GV.numTiles-1)]
                xPos in
    let tiles1 = List.sortBy .randI tiles in
    let tiles2 = List.indexedMap (\i a -> {a | randI = (toFloat i)}) tiles1 in
    tiles2

