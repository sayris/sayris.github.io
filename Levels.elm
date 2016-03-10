module Levels where 

import Spacing as Sp 
import GameBoard as GB 
import GlobalVars as GV
import ColorInterpolation as CI
import Matrix exposing (..) 
import BlendokuTiles as BT
import Random as R
import ColorInterpolation as CI
import Maybe
import Color as C

------------- Helper Functions -------------

levelToBSP : (Int, Int) -> ((Int, Int) -> List (Int, Int)) -> GB.GameBoard
levelToBSP dimens f =
    let pos = f dimens in
    List.indexedMap (\i p -> {index = i, pos = p, status = GB.Empty}) pos 

getPos : Int -> List a -> a
getPos n ls =
    case (n, ls) of 
        (0, a::aa) -> a 
        (_, a::aa) -> getPos (n-1) aa 
        (_, _) -> Debug.crash "getPos: passing in invalid index" 

fEq : Int -> Int -> Int 
fEq i offset = 
    Sp.fEq2 i (GV.tileSideLen//2) offset 

bEq : Int -> Int -> Int  
bEq i offset = 
    Sp.bEq2 i (GV.tileSideLen//2) offset 

genTileLevels : (Int, Int) -> List (Int, Int) -> R.Seed -> List BT.Tile
genTileLevels (w, h) iList sd = 
    let (colors, sd') = CI.colorMatrixS (6, 6) sd in 
    let c' = List.map (\p -> Maybe.withDefault C.white (get p colors)) iList in
    let nTiles = List.length c' in
    let pos = Sp.newSpaceTiles (w, h) nTiles GV.colWidth in
    BT.genTiles2 sd' (w, h) c' pos

genTiles2 : (Int, Int) -> Int -> R.Seed -> List BT.Tile
genTiles2 (w, h) lvl sd = 
    let (colors, sd') = CI.colorMatrixS (6, 6) sd in 
    let iList = getTileLevel lvl in 
    let c' = List.map (\p -> Maybe.withDefault C.white (get p colors)) iList in
    let nTiles = List.length c' in
    let pos = Sp.newSpaceTiles (w, h) nTiles 7 in
    BT.genTiles2 sd' (w, h) c' pos   

-- returns the function to generate the board
-- the Index list for grabbing the correct tiles 
-- and the list of "set points"
getLevel : Int -> (((Int, Int) -> GB.GameBoard), ((Int, Int) -> R.Seed -> List BT.Tile), List Int) 
getLevel lvl = 
    let msg = "getLevel: invalid level, picked " ++ (toString lvl) in 
    case lvl of 
        1 -> (level1BSP, level1Tiles, level1SetPts)
        2 -> (level2BSP, level2Tiles, level2SetPts)
        3 -> (level3BSP, level3Tiles, level3SetPts)
        4 -> (level4BSP, level4Tiles, level4SetPts)
        5 -> (level5BSP, level5Tiles, level5SetPts)
        6 -> (level6BSP, level6Tiles, level6SetPts)
        _ -> Debug.crash msg


getBoardPos : Int -> ((Int, Int) -> List (Int, Int)) 
getBoardPos lvl = 
    case lvl of 
        1 -> level1Pos'
        2 -> level2Pos
        3 -> level3Pos
        4 -> level4Pos
        5 -> level5Pos
        6 -> level6Pos
        _ -> Debug.crash "getBoardPos: invalid level, pick 1 - 6"

getTileLevel : Int -> List (Int, Int)
getTileLevel lvl = 
    case lvl of 
        1 -> level1IndexList
        2 -> level2IndexList
        3 -> level3IndexList
        4 -> level4IndexList
        5 -> level5IndexList
        6 -> level6IndexList
        _ -> Debug.crash "getTileLevel: erroneous level choice"

getSetTiles : Int -> List Int 
getSetTiles lvl = 
     case lvl of 
        1 -> level1SetPts
        2 -> level2SetPts
        3 -> level3SetPts
        4 -> level4SetPts
        5 -> level5SetPts
        6 -> level6SetPts
        _ -> Debug.crash "getSetTiles: erroneous level choice"
------------- Level 1 -------------

level1Pos' : (Int, Int) -> List (Int, Int)
level1Pos' (w, h) = 
    let nTiles = 6 in 
    let midW = w//2 in 
    let midH = ((h)//2) + 20 in 
    let m = nTiles // 2 in
    let latterPos = List.map (\i -> (midW, fEq i midH)) [1..m] in 
    let firstPos = List.map (\i -> (midW, bEq i midH)) [0..m] in
    let ls = List.append (List.reverse firstPos) latterPos in
    List.take nTiles ls

level1Pos : (Int, Int) -> Int -> List (Int, Int)
level1Pos (w, h) nTiles = 
    let midW = w//2 in 
    let midH = ((h)//2) + 20 in 
    let m = nTiles // 2 in
    let latterPos = List.map (\i -> (midW, fEq i midH)) [1..m] in 
    let firstPos = List.map (\i -> (midW, bEq i midH)) [0..m] in
    let ls = List.append (List.reverse firstPos) latterPos in
    List.take nTiles ls

level1BSP : (Int, Int) -> GB.GameBoard
level1BSP dims = 
    levelToBSP dims level1Pos' 



level1IndexList : List (Int, Int) 
level1IndexList =
    List.map (\i -> (i, 0)) [0..5]

level1SetPts : List Int 
level1SetPts = [0]

level1Tiles : (Int, Int) -> R.Seed -> List BT.Tile
level1Tiles dimens seed = 
    genTileLevels dimens level1IndexList seed 

------------- Level 2 -------------

level2Pos : (Int, Int) -> List (Int, Int) 
level2Pos (w, h) =
    let nTile = 5 in 
    let midLine = level1Pos (w, h) nTile in 
    let (x0, y0) = getPos 0 midLine in 
    let left = List.map (\i -> (bEq i x0, y0)) [2, 1] in 
    let right = List.map (\i -> (fEq i x0, y0)) [1, 2] in 
    List.append (List.append left midLine) right 


level2BSP : (Int, Int) -> GB.GameBoard
level2BSP dimens = 
    levelToBSP dimens level2Pos 


level2IndexList : List (Int, Int)
level2IndexList = 
    [ (0, 0) -- 0
    , (0, 1) -- 1
    , (0, 2) -- 2
    , (1, 2) -- 3
    , (2, 2) -- 4
    , (3, 2) -- 5
    , (4, 2) -- 6
    , (0, 3) -- 7
    , (0, 4) -- 8
    ]

level2SetPts : List Int 
level2SetPts = [1, 6, 8]

level2Tiles : (Int, Int) -> R.Seed -> List BT.Tile
level2Tiles dimens seed = 
    genTileLevels dimens level2IndexList seed 

------------- Level 3 -------------

level3Pos : (Int, Int) -> List (Int, Int) 
level3Pos (w, h) = 
    let nTile = 5 in 
    let midLine = level1Pos (w, h) nTile in 
    let (x0, y0) =getPos 3 midLine in
    let sEq i = ((GV.tileSideLen//2) * i) + (GV.tileSideLen * (i)) + x0 in
    let sEq2 i = x0 - (((GV.tileSideLen//2) * i) + (GV.tileSideLen * i)) in
    let left = List.map (\i -> (bEq i x0, y0)) [2, 1] in 
    let right = List.map (\i -> (fEq i x0, y0)) [1, 2] in
    List.append (List.append left midLine) right 

level3BSP : (Int, Int) -> GB.GameBoard
level3BSP dimens = 
    levelToBSP dimens level3Pos

level3IndexList : List (Int, Int) 
level3IndexList = 
    [ (3, 0) -- 0
    , (3, 1) -- 1
    , (0, 2) -- 2
    , (1, 2) -- 3
    , (2, 2) -- 4
    , (3, 2) -- 5
    , (4, 2) -- 6
    , (3, 3) -- 7
    , (3, 4) -- 8
    ]

level3SetPts : List Int 
level3SetPts = [3, 0]

level3Tiles : (Int, Int) -> R.Seed -> List BT.Tile
level3Tiles dimens seed = 
    genTileLevels dimens level3IndexList seed 

------------- Level 4 -------------

level4Pos : (Int, Int) -> List (Int, Int) 
level4Pos (w, h) = 
    let nTile = 4 in 
    let w1 = w//3 in 
    let verticalLine = level1Pos (w, h) nTile in 
    let leftLine = List.map (\(x, y) -> (w1, y)) verticalLine in 

    let (x0, y0) = getPos 0 leftLine in
    let sEq i = ((GV.tileSideLen//2) * i) + (GV.tileSideLen * i) + x0 in
    let top = List.map (\i -> (fEq i x0, y0)) [1..(nTile-1)] in 

    let (x3, y3) = getPos 3 leftLine in
    let bottom = List.map (\(x, y) -> (x, y3)) top in 

    let (xx3, yy3) = getPos 2 bottom in 
    let rightLine = List.map (\(x, y) -> (xx3, y)) verticalLine in 
    let rL = List.take 2 (List.drop 1 rightLine) in 
    List.foldl List.append rL [top, bottom, leftLine] 

level4BSP : (Int, Int) -> GB.GameBoard
level4BSP dimens = 
    levelToBSP dimens level4Pos

level4IndexList : List (Int, Int)
level4IndexList = 
    [ (0, 0) -- 0
    , (1, 0) -- 1
    , (2, 0) -- 2
    , (3, 0) -- 3
    , (3, 1) -- 4
    , (3, 2) -- 5
    , (3, 3) -- 6
    , (0, 1) -- 7
    , (0, 2) -- 8
    , (0, 3) -- 9
    , (1, 3) -- 10
    , (2, 3) -- 11
    ]

level4SetPts : List Int 
level4SetPts = [0, 3, 6, 9]

level4Tiles : (Int, Int) -> R.Seed -> List BT.Tile
level4Tiles dimens seed = 
    genTileLevels dimens level4IndexList seed 

------------- Level 5 -------------

level5Pos : (Int, Int) -> List (Int, Int) 
level5Pos (w, h) = 
    let nTile = 5 in 
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
    levelToBSP dimens level5Pos

level5IndexList : List (Int, Int) 
level5IndexList = 
    [ (0, 0) -- 0
    , (1, 0) -- 1
    , (2, 0) -- 2
    , (3, 0) -- 3
    , (4, 0) -- 4
    , (1, 2) -- 5
    , (2, 2) -- 6
    , (3, 2) -- 7
    , (2, 4) -- 8
    ] 

level5SetPts : List Int 
level5SetPts = [0, 4, 6]

level5Tiles : (Int, Int) -> R.Seed -> List BT.Tile
level5Tiles dimens seed = 
    genTileLevels dimens level5IndexList seed 

------------- Level 6 -------------

level6Pos : (Int, Int) -> List (Int, Int) 
level6Pos (w, h) = 
    let nTile = 4 in 
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
    levelToBSP dimens level6Pos

level6IndexList : List (Int, Int)
level6IndexList = 
    [ (1, 0) -- 0
    , (2, 0) -- 1
    , (3, 0) -- 2
    , (4, 0) -- 3
    , (2, 1) -- 4
    , (2, 2) -- 5
    , (2, 3) -- 6
    , (2, 4) -- 7
    , (1, 2) -- 8
    , (1, 4) -- 9
    , (0, 2) -- 10
    , (0, 3) -- 11
    , (0, 4) -- 12
    ]

level6SetPts : List Int 
level6SetPts = [3, 4, 10, 12]

level6Tiles : (Int, Int) -> R.Seed -> List BT.Tile
level6Tiles dimens seed = 
    genTileLevels dimens level6IndexList seed 