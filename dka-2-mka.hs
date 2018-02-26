import System.Environment
import System.IO
import Data.List

type State = String
type Symbol = String

data Transition = Transition {
                inputState :: State,
                symbol :: Symbol,
                outputState :: State
} deriving (Show)

data DKA = DKA {
                states :: [State],
                alphabet :: [Symbol],
                transitions :: [Transition],
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

parseTransitions :: [String] -> [Transition]
parseTransitions transitions = (map (\transition -> (Transition {
    inputState = (splitStringByPredicate (==',') transition)!!0,
    symbol = (splitStringByPredicate (==',') transition)!!1,
    outputState = (splitStringByPredicate (==',') transition)!!2
}))  transitions)

parseInput :: [String] -> DKA
parseInput lines = DKA {
    states = splitStringByPredicate (==',') (lines!!0),
    alphabet = getAlphabet (drop 3 lines),
    transitions = parseTransitions (drop 3 lines),
    initState = (lines!!1),
    endStates = splitStringByPredicate (==',') (lines!!2)
}

printTransition :: Transition -> String
printTransition transition = "    " ++ (inputState transition) ++ " -> " ++ (symbol transition) ++ " -> " ++ (outputState transition)

printDKA :: DKA -> IO ()
printDKA dka = do 
            putStr "States: "
            putStrLn (intercalate "," (states dka))
            putStr "Alphabet: "
            putStrLn (intercalate "," (alphabet dka))
            putStrLn "Transitions: "
            mapM_ putStrLn (map printTransition (transitions dka))  
            putStr "Init state: "
            putStrLn (initState dka)
            putStr "End states: "
            putStrLn (intercalate "," (endStates dka))

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
        "-i" -> printDKA parsedInput    
        "-t" -> print "Option -t"
        _ -> error "Invalid first argument."

        

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