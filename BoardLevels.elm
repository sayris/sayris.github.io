module BoardLevels where 

import GameBoard as GB 
import BlendokuTiles as BT
import Html exposing (..)
import Html.Attributes as HA
import Svg as S
import Svg.Attributes as SA
import Window as W 
import Maybe 
import GlovalVars as GV

------------- Helper Functions -------------

levelToBSP : (Int, Int) -> ((Int, Int) -> Int -> List (Int, Int)) -> Int -> GB.GameBoard
levelToBSP dimens f n =
    let pos = f dimens n in
    List.indexedMap (\i p -> {index = i, pos = p, status = GB.Empty}) pos 

getPos : Int -> List a -> a
getPos n ls =
    case (n, ls) of 
        (0, a::aa) -> a 
        (_, a::aa) -> getPos (n-1) aa 
        (_, _) -> Debug.crash "getPos: passing in invalid index" 

fEq : Int -> Int -> Int 
fEq i offset = 
    ((GV.tileSideLen//2) * i) + (GV.tileSideLen * (i)) + offset 

bEq : Int -> Int -> Int  
bEq i offset = 
     offset - (((GV.tileSideLen//2) * i) + (GV.tileSideLen * i))

------------- Level 1 -------------

level1Pos : (Int, Int) -> Int -> List (Int, Int)
level1Pos (w, h) GV.numTiles = 
    let midW = w//2 in 
    let midH = h//2 in 
    let m = GV.numTiles // 2 in
    let latterPos = List.map (\i -> (midW, fEq i midH)) [1..m] in 
    let firstPos = List.map (\i -> (midW, bEq i midH)) [0..m] in
    let ls = List.append (List.reverse firstPos) latterPos in
    List.take GV.numTiles ls

level1BSP : (Int, Int) -> GB.GameBoard
level1BSP dims = 
    levelToBSP dims level1Pos 6 

------------- Level 2 -------------

level2Pos : (Int, Int) -> Int -> List (Int, Int) 
level2Pos (w, h) nTile =
    let midLine = level1Pos (w, h) nTile in 
    let (x0, y0) = getPos 0 midLine in 
    let left = List.map (\i -> (bEq i x0, y0)) [2, 1] in 
    let right = List.map (\i -> (fEq i x0, y0)) [1, 2] in 
    List.append (List.append left midLine) right 


level2BSP : (Int, Int) -> GB.GameBoard
level2BSP dimens = 
    levelToBSP dimens level2Pos 5 


------------- Level 3 -------------

level3Pos : (Int, Int) -> Int -> List (Int, Int) 
level3Pos (w, h) nTile = 
    let midLine = level1Pos (w, h) nTile in 
    let (x0, y0) =getPos 3 midLine in
    let sEq i = ((GV.tileSideLen//2) * i) + (GV.tileSideLen * (i)) + x0 in
    let sEq2 i = x0 - (((GV.tileSideLen//2) * i) + (GV.tileSideLen * i)) in
    let left = List.map (\i -> (bEq i x0, y0)) [2, 1] in 
    let right = List.map (\i -> (fEq i x0, y0)) [1, 2] in
    List.append (List.append left midLine) right 

level3BSP : (Int, Int) -> GB.GameBoard
level3BSP dimens = 
    levelToBSP dimens level3Pos 5


------------- Level 4 -------------

level4Pos : (Int, Int) -> Int -> List (Int, Int) 
level4Pos (w, h) nTile = 
    let w1 = w//3 in 
    let verticalLine = level1Pos (w, h) nTile in 
    let leftLine = List.map (\(x, y) -> (w1, y)) verticalLine in 

    let (x0, y0) = getPos 0 leftLine in
    let sEq i = ((GV.tileSideLen//2) * i) + (GV.tileSideLen * i) + x0 in
    let top = List.map (\i -> (fEq i x0, y0)) [1, 2, 3] in 

    let (x3, y3) = getPos 3 leftLine in
    let bottom = List.map (\(x, y) -> (x, y3)) top in 

    let (xx3, yy3) = getPos 2 bottom in 
    let rightLine = List.map (\(x, y) -> (xx3, y)) verticalLine in 
    let rL = List.take 2 (List.drop 1 rightLine) in 
    List.foldl List.append rL [top, bottom, leftLine] 

level4BSP : (Int, Int) -> GB.GameBoard
level4BSP dimens = 
    levelToBSP dimens level4Pos 4

------------- Level 5 -------------

level5Pos : (Int, Int) -> Int -> List (Int, Int) 
level5Pos (w, h) nTile = 
    let w1 = w//3 in 
    let vl1 = level1Pos (w, h) nTile in 
    let vl2 = level1Pos (w, h) 3 in 
    let vl3 = level1Pos (w, h) 1 in 
    let ws = List.map (\i -> fEq i w1)  [1, 2, 3] in 
    let w0 = getPos 0 ws in
    let w1 = getPos 1 ws in
    let w2 = getPos 2 ws in
    let v1 = List.map (\(x, y) -> (w0, y)) vl1 in
    let v2 = List.map (\(x, y) -> (w1, y)) vl2 in
    let v3 = List.map (\(x, y) -> (w2, y)) vl3 in
    List.foldl List.append v3 [v2, v1] 

level5BSP : (Int, Int) -> GB.GameBoard
level5BSP dimens = 
    levelToBSP dimens level5Pos 5

------------- Level 6 -------------

level6Pos : (Int, Int) -> Int -> List (Int, Int) 
level6Pos (w, h) nTile = 
    let w1 = w//3 in 
    let vl1 = List.map (\(x,y) -> (w1, y)) (level1Pos (w, h) (nTile + 1)) in 
    let (x0, y0) = getPos 0 vl1 in  
    let (x1, y1) = getPos 1 vl1 in  
    let (x2, y2) = getPos 2 vl1 in  
    let hz2 = List.map (\i -> (fEq i x2, y2)) [1, 2, 3, 4] in 
    let hz1 = List.map (\i -> (fEq i x1, y1)) [2, 4] in 
    let hz0 = List.map (\i -> (fEq i x0, y0)) [2, 3, 4] in 
    let vl = List.drop 1 vl1 in 
    List.foldl List.append hz0 [hz1, hz2, vl]

level6BSP : (Int, Int) -> GB.GameBoard
level6BSP dimens = 
    levelToBSP dimens level6Pos 4




------------- Rendering the Board -------------

drawGameBoard : (Int, Int) -> Html
drawGameBoard (w, h) =
    let board = level6BSP (w, h) in 
    div []
        [
            S.svg
                [ SA.width  (toString w)
                , SA.height (toString h)
                ]
                (List.map GB.boardImage board)
        ]



main : Signal Html 
main =
    Signal.map drawGameBoard W.dimensions