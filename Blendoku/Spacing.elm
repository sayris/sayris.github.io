module Spacing where 
import List 
import GlobalVars as GV 
import Array as A
import Matrix exposing (..) 

fEq : Int -> Int -> Int -> Int 
fEq i gap offset = 
    (gap * (1+i)) + (GV.tileSideLen * i) + offset 

bEq : Int -> Int -> Int -> Int  
bEq i gap offset = 
     offset - ((gap * (2+i)) + (GV.tileSideLen * i))


fEq2 : Int -> Int -> Int -> Int 
fEq2 i gap offset = 
    (gap * i) + (GV.tileSideLen * i) + offset 

bEq2 : Int -> Int -> Int -> Int 
bEq2 i gap offset = 
     offset - ((gap * i) + (GV.tileSideLen * i))

spacing : (Int, Int) -> Int -> Int -> Int -> Bool -> List (Int, Int)
spacing (w, h) numTiles y gap isTile = 
    let midPoint = w//2 in 
    let midTile = numTiles//2 in
    let latterPos = List.map (\i -> (fEq i gap midPoint, y)) [0..midTile] in 
    let firstPos = List.map (\i -> (bEq i gap midPoint, y)) [0..(midTile-1)] in
    let firstPos' = if isTile then firstPos else (List.reverse firstPos) in
    let spaces = List.append firstPos' latterPos in 
    List.take numTiles spaces 

spacingBoard (w, h) numTiles =
    spacing (w, h) numTiles 250 (GV.tileSideLen//2) False

spacingTile (w, h) numTiles =
    spacing (w, h) numTiles 10 10 True

-- spacing for 1 dimension 
spc : Int -> Int -> Int -> A.Array Int 
spc numTiles gap offset =
    let eq i = fEq i gap offset in
    let tmp = List.map eq [0..(numTiles - 1)] in 
    A.fromList tmp 

calcOffset : Int -> Int -> Int -> List Int 
calcOffset num offset midpoint = 
    let mid = num//2 in 
    let before = [0..mid] in
    let after = [(mid+1)..(num-1)] in
    let forward = List.map (\i -> midpoint + (offset * i)) before in 
    let back = List.map (\i -> midpoint - (offset*i)) after in 
    let back' = List.reverse back in 
    List.append forward back'

tileOffset : Int -> Int
tileOffset gap = 
    GV.tileSideLen + (2 * gap)

-- spacing for 1 dimension (based on middle)
spcMid : Int -> Int -> Int -> A.Array Int 
spcMid numTiles gap midPoint = 
    let spaces = calcOffset numTiles (tileOffset gap) midPoint in 
    --let midTile = numTiles//2 in 
    --let latterPos = List.map (\i -> fEq i gap midPoint) [0..midTile] in 
    --let firstPos = List.map (\i -> bEq i gap midPoint) [0..(midTile-1)] in
    --let spaces = List.append firstPos (List.reverse latterPos) in 
    A.fromList (List.take numTiles spaces) 

newSpaceTiles : (Int, Int) -> Int -> Int -> List (Int, Int)
newSpaceTiles (w, h) numTiles colWidth = 
    let rows = ceiling ((toFloat numTiles)/(toFloat colWidth)) in
    let rows' = max 1 rows in  -- in case numTiles == 0 
    let tilesPerRow = ceiling ((toFloat numTiles)/(toFloat rows')) in
    let gap = 5 in 
    let offsetX = w//3 in
    let offsetY = 0 in 
    let spX = spc colWidth gap offsetX in
    let spY = spc rows' gap offsetY in
    let c0 = GV.justWrap (A.get 0 spX) in 
    let col0 = List.map (\i -> [(c0, GV.justWrap (A.get i spY))]) [0..(rows'-1)] in
    let rows = List.indexedMap (\i ts -> 
                List.append
                ts
                (List.map (\d -> (GV.justWrap (A.get d spX), GV.justWrap (A.get i spY))) [1..(tilesPerRow-1)])) col0 in 
    List.take numTiles (flatten (fromList rows)) 
