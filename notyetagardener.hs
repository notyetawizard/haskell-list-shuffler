import System.Random

--mkStdGen 20151129

{-
    Shuffle list function:
    used to randomly order which cells are up next for growth

    Steps:
    pass function two lists (probably one empty) and a StdGen
    get the length of the list being shuffled
    pick a number between (0 or 1)? and it
    append that element to the new list
    call recursively until shuffled list is empty
    return new list and StdGen 
-}

shuffle :: RandomGen g => ([a],[a]) -> g -> ([a],g)
shuffle ([],b) g = (b, g)
shuffle (a,b) g = 
    --get random position in a, and the new RandomGen
    let (n, ng) = randomR (0,length a -1) g
    --split a, move element to b, put a back together
        (a1, a2) = splitAt n a
        nb = head a2:b
        na = a1 ++ tail a2
    in shuffle (na, nb) ng
