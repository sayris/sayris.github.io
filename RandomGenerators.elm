module RandomGenerators where 

import Random 


genRIndex : Random.Seed -> Int -> (List Float, Random.Seed)
genRIndex s n =
    let floatList n = Random.list n (Random.float 0 1) in 
    Random.generate floatList s 