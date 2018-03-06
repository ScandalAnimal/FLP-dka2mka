import System.Environment
import System.IO
import Data.List
import Debug.Trace

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

sinkState = "sink"

splitStringByPredicate :: (Char -> Bool) -> String -> [String]
splitStringByPredicate predicate s = case dropWhile predicate s of
                                              "" -> []
                                              s' -> w : splitStringByPredicate predicate s''
                                                  where (w, s'') = break predicate s'

removeDuplicatesFromList :: (Eq a) => [a] -> [a]
removeDuplicatesFromList = foldr (\symbol cleanList -> if symbol `elem` cleanList then cleanList else symbol:cleanList) []

createAlphabet :: [String] -> [Symbol]
createAlphabet transitions = removeDuplicatesFromList (map (\transition -> (splitStringByPredicate (==',') transition) !! 1) transitions)

parseTransitions :: [String] -> [Transition]
parseTransitions transitions = (map (\transition -> (Transition {
    inputState = (splitStringByPredicate (==',') transition)!!0,
    symbol = (splitStringByPredicate (==',') transition)!!1,
    outputState = (splitStringByPredicate (==',') transition)!!2
}))  transitions)

parseInput :: [String] -> DKA
parseInput lines = DKA {
    states = splitStringByPredicate (==',') (lines!!0),
    alphabet = createAlphabet (drop 3 lines),
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

eliminateInaccessibleStates :: [Transition] -> ([State],[State]) -> ([State],[State])
eliminateInaccessibleStates transitions (x,y) = if (x == y)
                                                    then (x,y)
                                                    else let newStates = removeDuplicatesFromList (foldr (\transition list -> if (inputState transition) `elem` list 
                                                                                                                                  then (outputState transition):list 
                                                                                                                                  else list) (x) transitions)
                                                        in eliminateInaccessibleStates transitions (newStates,x)

createTransitionsToSinkState :: [Symbol] -> [State] -> [Transition] -> [Transition]
createTransitionsToSinkState alphabet states transitions = foldr (\transition cleanList -> Transition {
        inputState = fst transition,
        symbol = snd transition,
        outputState = sinkState
    }:cleanList) [] (removeDuplicatesFromList (allTransitions \\ existingTransitions))
    where allTransitions = [ (state,symbol) | state <- states, symbol <- alphabet]
          existingTransitions = foldr (\transition cleanList -> (inputState transition, symbol transition):cleanList) [] transitions

createReducedDKA :: DKA -> DKA
createReducedDKA inputDKA = DKA {
        states = if ((length transitionsToSinkState) == 0) then onlyAccessibleStates else sinkState:onlyAccessibleStates,
        alphabet = alphabet inputDKA,
        transitions = foldr (:) transitionsToSinkState transitionsFromAccessibleStates,
        initState = initState inputDKA,
        endStates = intersect (endStates inputDKA) onlyAccessibleStates
    }
    where onlyAccessibleStates = fst (eliminateInaccessibleStates (transitions inputDKA) ((:[]) (initState inputDKA),[]))
          transitionsFromAccessibleStates = filter (\transition -> inputState transition `elem` onlyAccessibleStates) (transitions inputDKA)
          transitionsToSinkState = createTransitionsToSinkState (alphabet inputDKA) onlyAccessibleStates transitionsFromAccessibleStates

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
        "-t" -> printDKA (createReducedDKA parsedDKA) 
        _ -> error "Invalid first argument."


    hClose handle
    return ()
