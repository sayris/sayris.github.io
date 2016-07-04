module ColorInterpolation where 

import Color exposing (..)
import List
import Random
import Signal
import Mouse
import Window
import Matrix exposing (..)

type alias ColorPair = (Color, Color)
type alias State = ColorPair
initState = (Color.white, Color.white)

interpolateEq : Float -> Int -> Int -> Int 
interpolateEq p c1 c2 =
    let c1Float = toFloat c1 in
    let c2Float = toFloat c2 in
    round ((c1Float * p) + (c2Float * (1-p)))

interpolateColors : Float -> Color -> Color -> Color 
interpolateColors p color1 color2 = 
    let c1 = toRgb color1 in
    let c2 = toRgb color2 in
    let r = interpolateEq p c1.red c2.red in
    let g = interpolateEq p c1.green c2.green in
    let b = interpolateEq p c1.blue c2.blue in
    rgb r g b

pruneList : List a -> List a
pruneList ls =
    let foo i acc ls =
        case ls of 
            [] -> acc
            a::aa -> if i%2 == 0 then foo (i+1) acc aa 
                     else foo (i+1) (a::acc) aa 
    in 
    List.reverse (foo 0 [] ls)

interpList2 : Int -> Color -> Color -> List Color
interpList2 num c1 c2 =
    let ll = [0..(num*2)] in
    let ps = List.map (\index -> index * (1/num)) ll in
    let c = List.map (\p -> interpolateColors p c1 c2) ps in 
    pruneList c

interpList : Int -> Color -> Color -> List Color
interpList num c1 c2 =
    let ll = [0..(num-1)] in
    let ps = List.map (\index -> index * (1/num)) ll in
    List.map (\p -> interpolateColors p c1 c2) ps

genColor : Random.Seed -> (ColorPair, Random.Seed)
genColor s =
    let colorGen = Random.map3 rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255) in
    let colorsGen = Random.pair colorGen colorGen in
    Random.generate colorsGen s

signalColorSeed : Signal a -> Signal (ColorPair, Random.Seed)
signalColorSeed sig =
    let genColorAndSeed (cs, seed) = genColor seed in
    let init = ((Color.white, Color.black), Random.initialSeed 3487) in
    Signal.foldp (\signal val -> genColorAndSeed val) init sig

signalColor : Signal a -> Signal ColorPair
signalColor sig =
    let extractColors (colors, seed) = colors in
    Signal.map extractColors (signalColorSeed sig)

------------------- Color Matrix ----------------------

colorMatrix : (Int, Int) -> Random.Seed -> Matrix Color 
colorMatrix (x, y) seed = 
    let ((c1, c2), seed2) = genColor seed in 
    let ((c3, c4), seed3) = genColor seed2 in
    let leftC = interpList y c1 c2 in 
    let rightC = interpList y c3 c4 in 
    let ls = List.map2 (\c k -> interpList x c k) leftC rightC in
    fromList ls 

colorMatrixS : (Int, Int) -> Random.Seed -> (Matrix Color, Random.Seed) 
colorMatrixS (x, y) seed = 
    let ((c1, c2), seed2) = genColor seed in 
    let ((c3, c4), seed3) = genColor seed2 in
    let leftC = interpList y c1 c2 in 
    let rightC = interpList y c3 c4 in 
    let ls = List.map2 (\c k -> interpList x c k) leftC rightC in
    (fromList ls, seed3) 
