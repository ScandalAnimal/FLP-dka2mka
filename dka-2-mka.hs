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

eliminateInaccessibleStates :: [Transition] -> [State] -> [State]
eliminateInaccessibleStates transitions init =
--    foldr (\transition list -> if (inputState transition) `elem` list then (outputState transition):list else list) ((:[]) init) transitions
    let computedStates = (foldr (\transition list -> if (inputState transition) `elem` list then (outputState transition):list else list) (init) transitions):init
    in if computedStates!!0 == computedStates!!1
        then computedStates!!0
        else (eliminateInaccessibleStates transitions (computedStates!!0)):[]


getReducedDKA :: DKA -> DKA
getReducedDKA inputDKA = DKA {
--        states = states inputDKA,
        states = eliminateInaccessibleStates (transitions inputDKA) ((:[]) (initState inputDKA)),
        alphabet = alphabet inputDKA,
--        transitions = filter (\transition -> inputState transition == initState inputDKA) (transitions inputDKA),
        transitions = transitions inputDKA,
        initState = initState inputDKA,
        endStates = endStates inputDKA
    }

main = do 
    arguments <- getArgs
    let argLength = length arguments
    handle <- case argLength of
                1 -> return stdin
                2 -> openFile (arguments!!1) ReadMode
                _ -> error "Invalid number of arguments."

    contents <- hGetContents handle
    let parsedDKA = parseInput (lines contents)
    case head arguments of
        "-i" -> printDKA parsedDKA    
        "-t" -> printDKA (getReducedDKA parsedDKA) 
        _ -> error "Invalid first argument."


    hClose handle
    return ()
