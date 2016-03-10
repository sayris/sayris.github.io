module GlobalVars where 

tileSideLen : Int 
tileSideLen = 45


numTiles : Int
numTiles = 7

colWidth : Int 
colWidth = 7  



justWrap : Maybe number -> number 
justWrap x = 
    Maybe.withDefault 0 x 

getCenterPos : (Int, Int) -> (Int, Int)
getCenterPos (x, y) =
    let adjX = x - (tileSideLen//3) in
    let adjY = y - (tileSideLen//3) in
    (adjX, adjY)