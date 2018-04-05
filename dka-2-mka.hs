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
    eqClass :: EqClass
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
    eqClassTempStateGroups :: [[State]]
} deriving (Show)

sinkState = "sink"

blankEqClass = EqClass {
    eqClassStateGroups = [],
    eqClassAlphabet = [],
    eqClassGroupTransitions = [],
    eqClassTempStateGroups = []
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
    eqClass = blankEqClass
}

printTransition :: Transition -> String
printTransition transition = "    " ++ (transitionInputState transition) ++ " -> " ++ (transitionSymbol transition) ++ " -> " ++ (transitionOutputState transition)

printMinimizedTransition :: Transition -> String
printMinimizedTransition transition = (transitionInputState transition) ++ "," ++ (transitionSymbol transition) ++ "," ++ (transitionOutputState transition)

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
    -- putStrLn "Debug part (delete me after)"
    -- print (eqClass dka)

printMinimizedDKA :: DKA -> IO ()
printMinimizedDKA dka = do 
    putStrLn (intercalate "," (states dka))
    putStrLn (initState dka)
    putStrLn (intercalate "," (endStates dka))
    mapM_ putStrLn (map printMinimizedTransition (transitions dka))  
    -- putStrLn "Debug part (delete me after)"
    -- print (eqClass dka)

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
        eqClass = blankEqClass
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
    where outputStates = (foldr (\transition list -> if (((transitionInputState transition) `elem` (groupTransitionInputStates groupTransition)) && ((==) (groupTransitionSymbol groupTransition) (transitionSymbol transition)))
                                   then (transitionOutputState transition):list
                                   else list) [] transitions)


iterateOverStateGroups :: [GroupTransition] -> [Transition] -> [GroupTransition]
iterateOverStateGroups [] transitions = []
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

splitGroupStates :: [(State,State)] -> [[State]] -> [[State]]
splitGroupStates zippedStates (stateGroup:[]) = (foldr (\tuple list -> if ((snd tuple) `elem` stateGroup) then (fst tuple):list else list ) [] (zippedStates)):[]
splitGroupStates zippedStates (stateGroup:xs) = (foldr (\tuple list -> if ((snd tuple) `elem` stateGroup) then (fst tuple):list else list ) [] (zippedStates)):splitGroupStates zippedStates xs

splitGroups :: Symbol -> EqClass -> Maybe Int -> [[State]]
splitGroups symbol eqClass indexToSplit = case indexToSplit of
                                     Nothing -> eqClassStateGroups eqClass
                                     Just i -> let filteredGroupTransitions = foldr (\transition list -> if ((==) symbol (groupTransitionSymbol transition)) then transition:list else list) [] (eqClassGroupTransitions eqClass)
                                                   groupTransition = (filteredGroupTransitions)!!i
                                                   inputStates = groupTransitionInputStates groupTransition
                                                   beforeStateGroups = foldr (\stateGroup list -> if ((==) stateGroup inputStates) then list else stateGroup:list) [] (eqClassStateGroups eqClass)
                                                   zippedStates = zip (groupTransitionInputStates groupTransition) (groupTransitionOutputStates groupTransition)
                                                   splittedStateGroup = splitGroupStates zippedStates (eqClassStateGroups eqClass)
                                               in foldr (:) beforeStateGroups splittedStateGroup

generateNewStateGroups :: [Symbol] -> EqClass -> [[State]]
generateNewStateGroups [] eqClass = []
generateNewStateGroups (symbol:[]) eqClass = let newStates = splitGroups symbol eqClass (getGroupToSplit (eqClassGroupTransitions eqClass) (getEqClassIndexes (eqClassStateGroups eqClass) (filterGroupTransitionsBySymbol (eqClassGroupTransitions eqClass) symbol)))
                                                 in if ((==) newStates (eqClassStateGroups eqClass))
                                                        then (eqClassStateGroups eqClass)
                                                        else newStates
generateNewStateGroups (symbol:xs) eqClass = let lastNewStates = generateNewStateGroups (symbol:[]) eqClass
                                                 in if ((==) lastNewStates (eqClassStateGroups eqClass))
                                                        then generateNewStateGroups xs eqClass
                                                        else lastNewStates

changeMe :: EqClass -> EqClass
changeMe eqClass = EqClass {
        eqClassStateGroups = eqClassStateGroups eqClass,
        eqClassAlphabet = eqClassAlphabet eqClass,
        eqClassGroupTransitions = eqClassGroupTransitions eqClass,
        eqClassTempStateGroups = filter (not . null) (generateNewStateGroups (eqClassAlphabet eqClass) eqClass)

    }

reduceDKA :: EqClass -> [Transition] -> EqClass
reduceDKA lastEquivalenceClass transitions = if ((==) (eqClassStateGroups lastEquivalenceClass) (eqClassTempStateGroups newEquivalenceClass)) 
                                                 then newEquivalenceClass
                                                 else reduceDKA EqClass {
                                                     eqClassStateGroups = eqClassTempStateGroups newEquivalenceClass,
                                                     eqClassAlphabet = eqClassAlphabet newEquivalenceClass,
                                                     eqClassGroupTransitions = [],
                                                     eqClassTempStateGroups = []
                                                 } transitions
                                                 -- else newEquivalenceClass
    where tuples = [(stateGroup,symbol) | stateGroup <- (eqClassStateGroups lastEquivalenceClass), symbol <- (eqClassAlphabet lastEquivalenceClass)]
          groupTransitions = iterateOverStateGroups (createGroupTransitionsFromTuples tuples) transitions
          tempEqClass = EqClass {
                            eqClassStateGroups = eqClassStateGroups lastEquivalenceClass,
                            eqClassAlphabet = eqClassAlphabet lastEquivalenceClass,
                            eqClassGroupTransitions = groupTransitions,
                            eqClassTempStateGroups = []
                        }              
          newEquivalenceClass = changeMe tempEqClass

isStateGroupInEndStates :: (Int,[State]) -> [State] -> [Int]
isStateGroupInEndStates stateGroup endStates = removeDuplicatesFromList( foldr (\endState list -> if (endState `elem` (snd stateGroup)) then (fst stateGroup):list else list) [] endStates)

isSubset :: (Int,[State]) -> [State] -> [Int] 
isSubset stateGroup states = removeDuplicatesFromList (foldr (\state list -> if (state `elem` (snd stateGroup)) then (fst stateGroup):list else list) [] states)

getNewTransition :: [(Int,[State])] -> GroupTransition -> Transition
getNewTransition stateGroups groupTransition = Transition {
        transitionInputState = show (head (head (filter (not . null) inputState))),
        transitionSymbol = groupTransitionSymbol groupTransition,
        transitionOutputState = show (head (head (filter (not . null) outputState)))
    }
    where inputState = foldr (\stateGroup list -> (isSubset stateGroup (sort (removeDuplicatesFromList (groupTransitionInputStates groupTransition)))):list) [] stateGroups
          outputState = foldr (\stateGroup list -> (isSubset stateGroup (sort (removeDuplicatesFromList (groupTransitionOutputStates groupTransition)))):list) [] stateGroups

getNewTransitions :: [(Int,[State])] -> [GroupTransition] -> [Transition]
getNewTransitions stateGroups groupTransitions = foldr (\groupTransition list -> (getNewTransition stateGroups groupTransition):list) [] groupTransitions 

convertEquivalenceClassToDKA :: DKA -> EqClass -> DKA
convertEquivalenceClassToDKA inputDKA eqClass = DKA {
        states = newStates,
        alphabet = alphabet inputDKA,
        transitions = newTransitions,
        initState = head newInitState,
        endStates = foldr (\state list -> (show (head state)):list) [] newEndStates,
        eqClass = eqClass
    }
    where newStates = foldr (\stateGroup list -> (show (fromJust (elemIndex stateGroup (eqClassStateGroups eqClass)))):list) [] (eqClassStateGroups eqClass)
          renamedStates = foldr (\stateGroup list -> (fromJust (elemIndex stateGroup (eqClassStateGroups eqClass)), stateGroup):list) [] (eqClassStateGroups eqClass)
          newInitState = foldr (\state list -> if ((initState inputDKA) `elem` (snd state)) then (show (fst state)):list else list) [] renamedStates
          newEndStates = filter (not . null) (foldr (\state list -> (isStateGroupInEndStates state (endStates inputDKA)):list) [] renamedStates)
          newTransitions = getNewTransitions renamedStates (eqClassGroupTransitions eqClass)

createReducedDKA :: DKA -> DKA
createReducedDKA inputDKA = convertEquivalenceClassToDKA inputDKA lastEquivalenceClass
    where zeroEquivalenceClass = EqClass {
              eqClassStateGroups = [(endStates inputDKA), ((states inputDKA) \\ (endStates inputDKA))],
              eqClassAlphabet = alphabet inputDKA,
              eqClassGroupTransitions = [],
              eqClassTempStateGroups = []
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
        "-t" -> printMinimizedDKA (createReducedDKA (createWellDefinedDKA parsedDKA)) 
        _ -> error "Invalid first argument."


    hClose handle
    return ()
