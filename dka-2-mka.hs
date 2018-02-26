import System.Environment
import System.IO

type State = String
type Symbol = String

data DKA = DKA {
                states :: [State],
                alphabet :: [Symbol],
                transitions :: [String],
                initState :: State,
                endStates :: [State]
} deriving (Show)

splitStringByPredicate :: (Char -> Bool) -> String -> [String]
splitStringByPredicate predicate s = case dropWhile predicate s of
                                              "" -> []
                                              s' -> w : splitStringByPredicate predicate s''
                                                  where (w, s'') = break predicate s'

removeDuplicatesFromList :: (Eq a) => [a] -> [a]
removeDuplicatesFromList = foldr (\symbol cleanList -> if symbol `elem` cleanList then cleanList else symbol:cleanList) []

getAlphabet :: [String] -> [Symbol]
getAlphabet transitions = removeDuplicatesFromList (map (\transition -> (splitStringByPredicate (==',') transition) !! 1) transitions)


parseInput :: [String] -> DKA
parseInput lines = DKA {
    states = splitStringByPredicate (==',') (lines!!0),
    alphabet = getAlphabet (drop 3 lines),
    transitions = drop 3 lines,
    initState = (lines!!1),
    endStates = splitStringByPredicate (==',') (lines!!2)
}

main = do 
    arguments <- getArgs
    let argLength = length arguments
    handle <- case argLength of
                1 -> return stdin
                2 -> openFile (arguments!!1) ReadMode
                _ -> error "Invalid number of arguments."

    contents <- hGetContents handle
    let parsedInput = parseInput (lines contents)
    case head arguments of
        "-i" -> print "Option -i"    
        "-t" -> print "Option -t"
        _ -> error "Invalid first argument."

    
    -- TODO parse input to structure representing FSM
    print parsedInput    

    hClose handle
    return ()

-- postup
-- nacitanie parametrov
-- kontrola validity poctu parametrov, u prveho parametru kontrola ze je to jedna z povolenych hodnoty
--	druhy parameter je nepovinny, ak tam je tak kontrola ze taky subor naozaj existuje
--nacitanie vstupu do nejakej struktury, vstup moze mat 2 rozne varianty:	
--	citanie zo stdin - ak neni specifikovany druhy parameter - nacitanie v loope az sa neklikne dvakrat enter
--	citanie zo suboru - az do konca suboru
--ak bola zvolena volba -i, tak vypis vstupu do nejakeho ineho vypisu - napriklad slovny popis ktore hodnoty su co

-- kontrola ze pociatocny a koncove stavy sa nachadzaju aj v zozname stavov