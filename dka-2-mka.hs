import System.Environment
import System.IO
import Data.List
import Debug.Trace

type State = String
type Symbol = String

data Transition = Transition {
    transitionInputState :: State,
    transitionSymbol :: Symbol,
    transitionOutputState :: State
} deriving (Show)

data DKA = DKA {
    states :: [State],
    alphabet :: [Symbol],
    transitions :: [Transition],
    initState :: State,
    endStates :: [State],
    toDelete :: EqClass
} deriving (Show)

data GroupTransition = GroupTransition {
    groupTransitionInputStates :: [State],
    groupTransitionSymbol :: Symbol,
    groupTransitionOutputStates :: [State]
} deriving (Show)

data EqClass = EqClass {
    eqClassStateGroups :: [[State]],
    eqClassAlphabet :: [Symbol],
    eqClassGroupTransitions :: [GroupTransition]
} deriving (Show)

sinkState = "sink"

blankEqClass = EqClass {
    eqClassStateGroups = [],
    eqClassAlphabet = [],
    eqClassGroupTransitions = []
}

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
    transitionInputState = (splitStringByPredicate (==',') transition)!!0,
    transitionSymbol = (splitStringByPredicate (==',') transition)!!1,
    transitionOutputState = (splitStringByPredicate (==',') transition)!!2
}))  transitions)

parseInput :: [String] -> DKA
parseInput lines = DKA {
    states = splitStringByPredicate (==',') (lines!!0),
    alphabet = createAlphabet (drop 3 lines),
    transitions = parseTransitions (drop 3 lines),
    initState = (lines!!1),
    endStates = splitStringByPredicate (==',') (lines!!2),
    toDelete = blankEqClass
}

printTransition :: Transition -> String
printTransition transition = "    " ++ (transitionInputState transition) ++ " -> " ++ (transitionSymbol transition) ++ " -> " ++ (transitionOutputState transition)

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
    putStrLn "Debug part (delete me after)"
    print (toDelete dka)

eliminateInaccessibleStates :: [Transition] -> ([State],[State]) -> ([State],[State])
eliminateInaccessibleStates transitions (x,y) = if (x == y)
                                                    then (x,y)
                                                    else let newStates = removeDuplicatesFromList (foldr (\transition list -> if (transitionInputState transition) `elem` list 
                                                                                                                                  then (transitionOutputState transition):list 
                                                                                                                                  else list) (x) transitions)
                                                        in eliminateInaccessibleStates transitions (newStates,x)

createTransitionsToSinkState :: [Symbol] -> [State] -> [Transition] -> [Transition]
createTransitionsToSinkState alphabet states transitions = foldr (\transition cleanList -> Transition {
        transitionInputState = fst transition,
        transitionSymbol = snd transition,
        transitionOutputState = sinkState
    }:cleanList) [] (removeDuplicatesFromList (missingTransitions))
    where allTransitions = [ (state,symbol) | state <- states, symbol <- alphabet]
          existingTransitions = foldr (\transition cleanList -> (transitionInputState transition, transitionSymbol transition):cleanList) [] transitions
          missingTransitions = filter (\transition -> not (transition `elem` existingTransitions)) allTransitions

createWellDefinedDKA :: DKA -> DKA
createWellDefinedDKA inputDKA = DKA {
        states = sort (if ((length transitionsToSinkState) == 0) then onlyAccessibleStates else sinkState:onlyAccessibleStates),
        alphabet = sort (intersect (alphabet inputDKA) alphabetFromAccessibleStates),
        transitions = foldr (:) transitionsToSinkState transitionsFromAccessibleStates,
        initState = initState inputDKA,
        endStates = sort (intersect (endStates inputDKA) onlyAccessibleStates),
        toDelete = blankEqClass
    }
    where onlyAccessibleStates = fst (eliminateInaccessibleStates (transitions inputDKA) ((:[]) (initState inputDKA),[]))
          transitionsFromAccessibleStates = filter (\transition -> transitionInputState transition `elem` onlyAccessibleStates) (transitions inputDKA)
          alphabetFromAccessibleStates = foldr (\transition cleanList -> (transitionSymbol transition):cleanList) [] transitionsFromAccessibleStates
          transitionsToSinkState = createTransitionsToSinkState alphabetFromAccessibleStates onlyAccessibleStates transitionsFromAccessibleStates

-- checkStateGroup :: ([State],Symbol) -> [Transition] -> [State]
-- checkStateGroup stateGroup transitions = removeDuplicatesFromList( 
--     foldr (\transition list -> if (((inputState transition) `elem` (fst stateGroup)) && ((==) (snd stateGroup) (symbol transition)))
--                                    then (outputState transition):list
--                                    else list
--       ) [] transitions)

-- iterateOverStateGroups :: [([State],Symbol)] -> [Transition] -> [(([State],Symbol),[State])]
-- iterateOverStateGroups (x:[]) transitions = (x,(checkStateGroup x transitions)) : []
-- iterateOverStateGroups (x:xs) transitions = (x,(checkStateGroup x transitions)) : (iterateOverStateGroups xs transitions)

-- -- temp return type
-- reduceDKA :: EqClass -> [Transition] -> [(([State],Symbol),[State])]
-- reduceDKA zeroEquivalenceClass transitions = iterateOverStateGroups [(stateGroup,symbol) | stateGroup <- (stateGroups zeroEquivalenceClass), symbol <- (alphabet zeroEquivalenceClass)] transitions

createGroupTransitionsFromTuples :: [([State],Symbol)] -> [GroupTransition]
createGroupTransitionsFromTuples tuples = foldr (\tuple list -> GroupTransition {
                                                                    groupTransitionInputStates = (fst tuple), 
                                                                    groupTransitionSymbol = (snd tuple), 
                                                                    groupTransitionOutputStates = []
                                                                }:list) [] tuples

checkStateGroup_ :: GroupTransition -> [Transition] -> GroupTransition
checkStateGroup_ groupTransition transitions = 
    GroupTransition {
        groupTransitionInputStates = groupTransitionInputStates groupTransition, 
        groupTransitionSymbol = groupTransitionSymbol groupTransition, 
        groupTransitionOutputStates = outputStates
    }
    where outputStates = removeDuplicatesFromList(foldr (\transition list -> if (((transitionInputState transition) `elem` (groupTransitionInputStates groupTransition)) && ((==) (groupTransitionSymbol groupTransition) (transitionSymbol transition)))
                                   then (transitionOutputState transition):list
                                   else list) [] transitions)


iterateOverStateGroups_ :: [GroupTransition] -> [Transition] -> [GroupTransition]
iterateOverStateGroups_ (x:[]) transitions = (checkStateGroup_ x transitions) : []
iterateOverStateGroups_ (x:xs) transitions = (checkStateGroup_ x transitions) : (iterateOverStateGroups_ xs transitions)


reduceDKA :: EqClass -> [Transition] -> EqClass
reduceDKA zeroEquivalenceClass transitions = 
    EqClass {
        eqClassStateGroups = eqClassStateGroups zeroEquivalenceClass,
        eqClassAlphabet = eqClassAlphabet zeroEquivalenceClass,
        eqClassGroupTransitions = iterateOverStateGroups_ (createGroupTransitionsFromTuples tuples) transitions
    }
    where tuples = [(stateGroup,symbol) | stateGroup <- (eqClassStateGroups zeroEquivalenceClass), symbol <- (eqClassAlphabet zeroEquivalenceClass)]

createReducedDKA :: DKA -> DKA
createReducedDKA inputDKA = DKA {
        --states = states inputDKA,
        states = states inputDKA,
        alphabet = alphabet inputDKA,
        transitions = transitions inputDKA,
        initState = initState inputDKA,
        endStates = endStates inputDKA,
        toDelete = lastEquivalenceClass
    }
    where zeroEquivalenceClass = EqClass {
              eqClassStateGroups = [(endStates inputDKA), ((states inputDKA) \\ (endStates inputDKA))],
              eqClassAlphabet = alphabet inputDKA,
              eqClassGroupTransitions = []
          }
          lastEquivalenceClass = reduceDKA zeroEquivalenceClass ((transitions inputDKA))

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
        "-t" -> printDKA (createReducedDKA (createWellDefinedDKA parsedDKA)) 
        _ -> error "Invalid first argument."


    hClose handle
    return ()
