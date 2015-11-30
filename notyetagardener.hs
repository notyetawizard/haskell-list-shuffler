import System.Random

--let seed = mkStdGen 098234507
--in ghci to test things

--Pull items randomly out of the first list and place them
--at the head of the second. Return with new RandomGen
shuffle :: RandomGen g => ([a],[a]) -> g -> ([a],g)
shuffle ([],b) g = (b, g)
shuffle (a,b) g = 
    --get random position in a, and a new RandomGen
    let (n, ng) = randomR (0,length a -1) g
    --split a, move element to b, put a back together
        (a1, a2) = splitAt n a
        nb = head a2:b
        na = a1 ++ tail a2
    in shuffle (na, nb) ng
