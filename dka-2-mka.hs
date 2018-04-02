import System.Environment
import System.IO
import Data.List
import Data.Maybe
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
    eqClassGroupTransitions :: [GroupTransition],
    deleteMe :: [[[State]]]
} deriving (Show)

sinkState = "sink"

blankEqClass = EqClass {
    eqClassStateGroups = [],
    eqClassAlphabet = [],
    eqClassGroupTransitions = [],
    deleteMe = []
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

createGroupTransitionsFromTuples :: [([State],Symbol)] -> [GroupTransition]
createGroupTransitionsFromTuples tuples = foldr (\tuple list -> GroupTransition {
                                                                    groupTransitionInputStates = (fst tuple), 
                                                                    groupTransitionSymbol = (snd tuple), 
                                                                    groupTransitionOutputStates = []
                                                                }:list) [] tuples

checkStateGroup :: GroupTransition -> [Transition] -> GroupTransition
checkStateGroup groupTransition transitions = 
    GroupTransition {
        groupTransitionInputStates = groupTransitionInputStates groupTransition, 
        groupTransitionSymbol = groupTransitionSymbol groupTransition, 
        groupTransitionOutputStates = outputStates
    }
    where outputStates = removeDuplicatesFromList(foldr (\transition list -> if (((transitionInputState transition) `elem` (groupTransitionInputStates groupTransition)) && ((==) (groupTransitionSymbol groupTransition) (transitionSymbol transition)))
                                   then (transitionOutputState transition):list
                                   else list) [] transitions)


iterateOverStateGroups :: [GroupTransition] -> [Transition] -> [GroupTransition]
iterateOverStateGroups (x:[]) transitions = (checkStateGroup x transitions) : []
iterateOverStateGroups (x:xs) transitions = (checkStateGroup x transitions) : (iterateOverStateGroups xs transitions)

filterGroupTransitionsBySymbol :: [GroupTransition] -> Symbol -> [GroupTransition]
filterGroupTransitionsBySymbol transitions symbol = filter (\transition -> (==) symbol (groupTransitionSymbol transition)) transitions

findIndexOfElementInStateGroups :: State -> [[State]] -> Int
findIndexOfElementInStateGroups state stateGroups = fromJust (head indexArray)
    where indexArray = foldr (\stateGroup list -> if (state `elem` stateGroup) then (elemIndex stateGroup stateGroups):list else list) [] stateGroups

getEqClassIndexes :: [[State]] -> [GroupTransition] -> [[Int]]
getEqClassIndexes stateGroups (transition:[]) = (foldr (\state list -> (findIndexOfElementInStateGroups state stateGroups):list) [] (groupTransitionOutputStates transition)):[]
getEqClassIndexes stateGroups (transition:xs) = (foldr (\state list -> (findIndexOfElementInStateGroups state stateGroups):list) [] (groupTransitionOutputStates transition)):(getEqClassIndexes stateGroups xs)

getGroupToSplit :: [GroupTransition] -> [[Int]] -> Maybe Int
getGroupToSplit groupTransitions indexes = if ((length indexList) /= 0)
                                             then head indexList
                                             else Nothing
    where indexList = (foldr (\index list -> if ((length (removeDuplicatesFromList index)) /= 1)
                                                             then (elemIndex index indexes):list
                                                             else list
           ) [] indexes)

-- https://www.reddit.com/r/haskell/comments/22o44v/delete_nth_item_another_noob_post/?st=jfib8isf&sh=9e1fe03b
deleteFromListAtIndex :: Int -> [a] -> [a]
deleteFromListAtIndex _ [] = []
deleteFromListAtIndex index (x:xs)
    | index == 0 = xs
    | otherwise = x : deleteFromListAtIndex (index - 1) xs

splitGroupStates :: GroupTransition -> [[State]] -> [[State]]
splitGroupStates groupTransition (stateGroup:[]) = (foldr (\tuple list -> if (snd tuple `elem` stateGroup) then (fst tuple):list else list ) [] (tuples)):[]
    where tuples = [(inputState,outputState) | inputState <- groupTransitionInputStates groupTransition, outputState <- groupTransitionOutputStates groupTransition]
splitGroupStates groupTransition (stateGroup:xs) = (foldr (\tuple list -> if (snd tuple `elem` stateGroup) then (fst tuple):list else list ) [] (tuples)):splitGroupStates groupTransition xs
    where tuples = [(inputState,outputState) | inputState <- groupTransitionInputStates groupTransition, outputState <- groupTransitionOutputStates groupTransition]

splitGroups :: EqClass -> Maybe Int -> [[State]]
splitGroups eqClass indexToSplit = case indexToSplit of
                                     Nothing -> eqClassStateGroups eqClass
                                     Just i -> let groupTransition = (eqClassGroupTransitions eqClass)!!i
                                                   inputStates = groupTransitionInputStates groupTransition
                                                   beforeStateGroups = foldr (\stateGroup list -> if ((==) stateGroup inputStates) then list else stateGroup:list) [] (eqClassStateGroups eqClass)
                                                   splittedStateGroup = splitGroupStates groupTransition (eqClassStateGroups eqClass)
                                               in foldr (:) beforeStateGroups splittedStateGroup

changeMe :: EqClass -> [[[State]]]
changeMe eqClass = foldr (\symbol list -> (splitGroups eqClass (getGroupToSplit (eqClassGroupTransitions eqClass) (getEqClassIndexes stateGroups (filterGroupTransitionsBySymbol groupTransitions symbol)))):list 
    ) [] (eqClassAlphabet eqClass)
    where stateGroups = eqClassStateGroups eqClass
          groupTransitions = eqClassGroupTransitions eqClass

reduceDKA :: EqClass -> [Transition] -> EqClass
reduceDKA zeroEquivalenceClass transitions = 
    EqClass {
        eqClassStateGroups = eqClassStateGroups zeroEquivalenceClass,
        eqClassAlphabet = eqClassAlphabet zeroEquivalenceClass,
        eqClassGroupTransitions = groupTransitions,
        deleteMe = changeme
    }
    where tuples = [(stateGroup,symbol) | stateGroup <- (eqClassStateGroups zeroEquivalenceClass), symbol <- (eqClassAlphabet zeroEquivalenceClass)]
          groupTransitions = iterateOverStateGroups (createGroupTransitionsFromTuples tuples) transitions
          tempEqClass = EqClass {
                            eqClassStateGroups = eqClassStateGroups zeroEquivalenceClass,
                            eqClassAlphabet = eqClassAlphabet zeroEquivalenceClass,
                            eqClassGroupTransitions = groupTransitions,
                            deleteMe = []
                        }
          -- todo, vrati to eqclass, porovnat s povodnou, a ked tak zaiterovat ak su rozne              
          changeme = changeMe tempEqClass

createReducedDKA :: DKA -> DKA
createReducedDKA inputDKA = DKA {
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
              eqClassGroupTransitions = [],
              deleteMe = []
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
