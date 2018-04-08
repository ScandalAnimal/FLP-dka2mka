-- FLP - FUN - DKA-2-MKA
-- xvasil02
-- Maroš Vasilišin
import System.Environment
import System.IO
import Data.List
import Data.Maybe

-- pomenovane typy pre stav a symbol, pre lepsiu orientaciu v kode
type State = String
type Symbol = String

-- datova struktura reprezentujuca prechod DKA
-- obsahuje vstupny stav, symbol a vystupny stav
data Transition = Transition {
    transitionInputState :: State,
    transitionSymbol :: Symbol,
    transitionOutputState :: State
} deriving (Show)

-- datova struktura reprezentujuca DKA
-- obsahuje stavy, abecedu, prechody, pociatocny stav, koncove stavy a
-- strukturu EqClass ktora vznikne z posledneho kroku minimalizacie DKA
data DKA = DKA {
    states :: [State],
    alphabet :: [Symbol],
    transitions :: [Transition],
    initState :: State,
    endStates :: [State],
    eqClass :: EqClass
} deriving (Show)

-- datova struktura reprezentujuca prechod medzi ekv. triedami v procese minimalizacie
data GroupTransition = GroupTransition {
    groupTransitionInputStates :: [State],
    groupTransitionSymbol :: Symbol,
    groupTransitionOutputStates :: [State]
} deriving (Show)

-- datova struktura pre minimalizaciu, reprezentuje ekvivalencnu triedu
data EqClass = EqClass {
    eqClassStateGroups :: [[State]],
    eqClassAlphabet :: [Symbol],
    eqClassGroupTransitions :: [GroupTransition],
    eqClassTempStateGroups :: [[State]]
} deriving (Show)

-- konstanta pre sink stav
sinkState = "sink"

-- prazdna EqClass pre inicializaciu
blankEqClass = EqClass {
    eqClassStateGroups = [],
    eqClassAlphabet = [],
    eqClassGroupTransitions = [],
    eqClassTempStateGroups = []
}

-- rozdeli string do pola podla prveho parametru, ktorym je predikat, 
-- napriklad podla toho ci je dany znak ciarka
splitStringByPredicate :: (Char -> Bool) -> String -> [String]
splitStringByPredicate predicate string = 
    case dropWhile predicate string of
        "" -> []
        notEmptyString -> before:splitStringByPredicate predicate after
            where (before,after) = break predicate notEmptyString

-- zo zoznamu zmaze duplikaty, kazdy prvok ponecha len 1x
removeDuplicatesFromList :: (Eq a) => [a] -> [a]
removeDuplicatesFromList = 
    foldr (\symbol cleanList -> 
        if symbol `elem` cleanList 
            then cleanList 
            else symbol:cleanList
    ) []

-- zo zoznamu prechodov vyberie len symboly abecedy
createAlphabet :: [String] -> [Symbol]
createAlphabet transitions = 
    removeDuplicatesFromList (map (\transition -> 
        let splittedTransition = (splitStringByPredicate (==',') transition)
        in if ((==) 3 (length splittedTransition))
            then ((splitStringByPredicate (==',') transition)!!1)
            else ""
    ) transitions)

-- zoznam prechodov zo vstupu sparsuje do pola struktur Transition
parseTransitions :: [String] -> [Transition]
parseTransitions transitions = (map (\transition -> 
    (Transition {
        transitionInputState = (splitStringByPredicate (==',') transition)!!0,
        transitionSymbol = (splitStringByPredicate (==',') transition)!!1,
        transitionOutputState = (splitStringByPredicate (==',') transition)!!2
    })
    )  transitions)

-- sparsuje vstup do struktury DKA
parseInput :: [String] -> DKA
parseInput lines = DKA {
    states = splitStringByPredicate (==',') (lines!!0),
    alphabet = createAlphabet (drop 3 lines),
    transitions = parseTransitions (drop 3 lines),
    initState = (lines!!1),
    endStates = splitStringByPredicate (==',') (lines!!2),
    eqClass = blankEqClass
}

-- vypise jeden prechod na vystup
printTransition :: Transition -> String
printTransition transition = "    " ++ (transitionInputState transition) ++ " -> " 
                            ++ (transitionSymbol transition) ++ " -> " 
                            ++ (transitionOutputState transition)

-- vypise jeden prechod minimalizovaneho DKA na vystup
printMinimizedTransition :: Transition -> String
printMinimizedTransition transition = (transitionInputState transition) ++ "," 
                            ++ (transitionSymbol transition) ++ "," 
                            ++ (transitionOutputState transition)

-- vypis automatu pri prepinaci -i
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

-- vypis minimalizovaneho automatu pri prepinaci -t
printMinimizedDKA :: DKA -> IO ()
printMinimizedDKA dka = do 
    putStrLn (intercalate "," (states dka))
    putStrLn (initState dka)
    putStrLn (intercalate "," (endStates dka))
    mapM_ putStrLn (map printMinimizedTransition (transitions dka))  

-- zmaze nedostupne stavy automatu
eliminateInaccessibleStates :: [Transition] -> ([State],[State]) -> ([State],[State])
eliminateInaccessibleStates transitions (x,y) = 
    if (x == y)
        then (x,y)
        else let newStates = removeDuplicatesFromList (foldr (\transition list -> if (transitionInputState transition) `elem` list 
                                                                                      then (transitionOutputState transition):list 
                                                                                      else list) (x) transitions)
             in eliminateInaccessibleStates transitions (newStates,x)

-- doplni chybajuce prechody do sink stavu
createTransitionsToSinkState :: [Symbol] -> [State] -> [Transition] -> [Transition]
createTransitionsToSinkState alphabet states transitions = foldr (\transition cleanList -> Transition {
        transitionInputState = fst transition,
        transitionSymbol = snd transition,
        transitionOutputState = sinkState
    }:cleanList) [] (removeDuplicatesFromList (missingTransitionsWithSinkState))
    where allTransitions = [(state,symbol) | state <- states, symbol <- alphabet]
          existingTransitions = foldr (\transition cleanList -> (transitionInputState transition, transitionSymbol transition):cleanList) [] transitions
          missingTransitions = filter (\transition -> not (transition `elem` existingTransitions)) allTransitions
          missingTransitionsWithSinkState = if ((length missingTransitions) /= 0)
                  then (foldr(\symbol list -> (sinkState, symbol):list) missingTransitions alphabet)
                  else missingTransitions

-- doplni do automatu sink stav, a jeho prechody, ak je to potrebne a odstrani nedostupne stavy
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

-- z dvojic pole stavov a symbol vytvori pole struktur GroupTransition, kde vystupne stavy budu prazdne
createGroupTransitionsFromTuples :: [([State],Symbol)] -> [GroupTransition]
createGroupTransitionsFromTuples tuples = foldr (\tuple list -> GroupTransition {
                                                                    groupTransitionInputStates = (fst tuple), 
                                                                    groupTransitionSymbol = (snd tuple), 
                                                                    groupTransitionOutputStates = []
                                                                }:list) [] tuples

-- vygeneruje vystupne stavy pre kazdu skupinu stavov v ekvivalencnej triede
createOutputStates :: GroupTransition -> [Transition] -> GroupTransition
createOutputStates groupTransition transitions = 
    GroupTransition {
        groupTransitionInputStates = groupTransitionInputStates groupTransition, 
        groupTransitionSymbol = groupTransitionSymbol groupTransition, 
        groupTransitionOutputStates = outputStates
    }
    where outputStates = (foldr (\transition list -> if (((transitionInputState transition) `elem` (groupTransitionInputStates groupTransition)) && ((==) (groupTransitionSymbol groupTransition) (transitionSymbol transition)))
                                   then (transitionOutputState transition):list
                                   else list) [] transitions)

-- iteracia cez pole skupin stavov v ekvivalencnej triede, pre kazdu sa generuje pole vystupnych stavov z pola prechodov
iterateOverStateGroups :: [GroupTransition] -> [Transition] -> [GroupTransition]
iterateOverStateGroups [] transitions = []
iterateOverStateGroups (x:[]) transitions = (createOutputStates x transitions) : []
iterateOverStateGroups (x:xs) transitions = (createOutputStates x transitions) : (iterateOverStateGroups xs transitions)

-- z pola prechodov vyberie len tie s vybranym symbolom
filterGroupTransitionsBySymbol :: [GroupTransition] -> Symbol -> [GroupTransition]
filterGroupTransitionsBySymbol transitions symbol = filter (\transition -> (==) symbol (groupTransitionSymbol transition)) transitions

-- v poli skupin stavov najde index tej skupiny, v ktorej sa nachadza vybrany stav
findIndexOfElementInStateGroups :: State -> [[State]] -> Int
findIndexOfElementInStateGroups state stateGroups = fromJust (head indexArray)
    where indexArray = foldr (\stateGroup list -> if (state `elem` stateGroup) then (elemIndex stateGroup stateGroups):list else list) [] stateGroups

--rekurzivne najde indexy skupin stavov ekvivalencnych tried
getEqClassIndexes :: [[State]] -> [GroupTransition] -> [[Int]]
getEqClassIndexes stateGroups (transition:[]) = (foldr (\state list -> (findIndexOfElementInStateGroups state stateGroups):list) [] (groupTransitionOutputStates transition)):[]
getEqClassIndexes stateGroups (transition:xs) = (foldr (\state list -> (findIndexOfElementInStateGroups state stateGroups):list) [] (groupTransitionOutputStates transition)):(getEqClassIndexes stateGroups xs)

-- najde skupinu stavov, ktora v danej ekvivalencnej triede musi byt rozdelena na dve skupiny
getGroupToSplit :: [GroupTransition] -> [[Int]] -> Maybe Int
getGroupToSplit groupTransitions indexes = if ((length indexList) /= 0)
                                             then head indexList
                                             else Nothing
    where indexList = (foldr (\index list -> if ((length (removeDuplicatesFromList index)) /= 1)
                                                             then (elemIndex index indexes):list
                                                             else list
           ) [] indexes)

-- zmaze prvok zo zoznamu na danom mieste
-- funkcia je inspiraciou z existujucej na adrese: https://www.reddit.com/r/haskell/comments/22o44v/delete_nth_item_another_noob_post/?st=jfib8isf&sh=9e1fe03b
deleteFromListAtIndex :: Int -> [a] -> [a]
deleteFromListAtIndex _ [] = []
deleteFromListAtIndex index (x:xs)
    | index == 0 = xs
    | otherwise = x : deleteFromListAtIndex (index - 1) xs


-- rozdeli zoznam stavov v ekv. triede do skupin stavov 
splitGroupStates :: [(State,State)] -> [[State]] -> [[State]]
splitGroupStates zippedStates (stateGroup:[]) = (foldr (\tuple list -> if ((snd tuple) `elem` stateGroup) then (fst tuple):list else list ) [] (zippedStates)):[]
splitGroupStates zippedStates (stateGroup:xs) = (foldr (\tuple list -> if ((snd tuple) `elem` stateGroup) then (fst tuple):list else list ) [] (zippedStates)):splitGroupStates zippedStates xs

-- rozdeli vybranu skupinu stavov, ktora s danym symbolom ma prechody do viacerych skupin
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

-- prechadza cez vsetky symboly a vsetky skupiny stavov a hlada skupinu ktoru je treba rozdelit
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

-- vygeneruje novu ekvivalencnu triedu
getNewEqClass :: EqClass -> EqClass
getNewEqClass eqClass = EqClass {
        eqClassStateGroups = eqClassStateGroups eqClass,
        eqClassAlphabet = eqClassAlphabet eqClass,
        eqClassGroupTransitions = eqClassGroupTransitions eqClass,
        eqClassTempStateGroups = filter (not . null) (generateNewStateGroups (eqClassAlphabet eqClass) eqClass)
    }

-- zredukuje povodny automat na minimalny
reduceDKA :: EqClass -> [Transition] -> EqClass
reduceDKA lastEquivalenceClass transitions = if ((==) (eqClassStateGroups lastEquivalenceClass) (eqClassTempStateGroups newEquivalenceClass)) 
                                                 then newEquivalenceClass
                                                 else reduceDKA EqClass {
                                                     eqClassStateGroups = eqClassTempStateGroups newEquivalenceClass,
                                                     eqClassAlphabet = eqClassAlphabet newEquivalenceClass,
                                                     eqClassGroupTransitions = [],
                                                     eqClassTempStateGroups = []
                                                 } transitions
    where tuples = [(stateGroup,symbol) | stateGroup <- (eqClassStateGroups lastEquivalenceClass), symbol <- (eqClassAlphabet lastEquivalenceClass)]
          groupTransitions = iterateOverStateGroups (createGroupTransitionsFromTuples tuples) transitions
          tempEqClass = EqClass {
                            eqClassStateGroups = eqClassStateGroups lastEquivalenceClass,
                            eqClassAlphabet = eqClassAlphabet lastEquivalenceClass,
                            eqClassGroupTransitions = groupTransitions,
                            eqClassTempStateGroups = []
                        }              
          newEquivalenceClass = getNewEqClass tempEqClass

-- kontrola ci dana skupina stavov obsahuje koncovy stav
isStateGroupInEndStates :: (Int,[State]) -> [State] -> [Int]
isStateGroupInEndStates stateGroup endStates = removeDuplicatesFromList( foldr (\endState list -> 
    if (endState `elem` (snd stateGroup)) 
        then (fst stateGroup):list 
        else list
    ) [] endStates)

-- kontrola ci pole stavov je podmnozinou ineho pola stavov.. vracia index pola, ktoreho je podmnozinou
isSubset :: (Int,[State]) -> [State] -> [Int] 
isSubset stateGroup states = removeDuplicatesFromList (foldr (\state list -> 
    if (state `elem` (snd stateGroup)) 
        then (fst stateGroup):list 
        else list
    ) [] states)

-- vytvori strukturu Transition zo struktury GroupTransition pre vypis
getNewTransition :: [(Int,[State])] -> GroupTransition -> Transition
getNewTransition stateGroups groupTransition = Transition {
        transitionInputState = show (head (head (filter (not . null) inputState))),
        transitionSymbol = groupTransitionSymbol groupTransition,
        transitionOutputState = show (head (head (filter (not . null) outputState)))
    }
    where inputState = foldr (\stateGroup list -> (isSubset stateGroup (sort (removeDuplicatesFromList (groupTransitionInputStates groupTransition)))):list) [] stateGroups
          outputState = foldr (\stateGroup list -> (isSubset stateGroup (sort (removeDuplicatesFromList (groupTransitionOutputStates groupTransition)))):list) [] stateGroups

-- vytvori nove struktury Transition z GroupTransition
getNewTransitions :: [(Int,[State])] -> [GroupTransition] -> [Transition]
getNewTransitions stateGroups groupTransitions = foldr (\groupTransition list -> (getNewTransition stateGroups groupTransition):list) [] groupTransitions 

-- ku kazdej skupine stavov priradi cislo, zacina od 1
addNumbersToStateGroups :: [[State]] -> [(Int,[State])]
addNumbersToStateGroups stateGroups = zip [1..] . sort . map sort $ stateGroups

-- strukturu EqClass skonvertuje na strukturu DKA pre vypis
convertEquivalenceClassToDKA :: DKA -> EqClass -> DKA
convertEquivalenceClassToDKA inputDKA eqClass = DKA {
        states = newStates,
        alphabet = alphabet inputDKA,
        transitions = newTransitions,
        initState = head newInitState,
        endStates = foldr (\state list -> (show (head state)):list) [] newEndStates,
        eqClass = eqClass
    }
    where renamedStates = addNumbersToStateGroups (eqClassStateGroups eqClass)
          newStates = foldr (\state list -> (show (fst state)):list) [] renamedStates
          newInitState = foldr (\state list -> if ((initState inputDKA) `elem` (snd state)) then (show (fst state)):list else list) [] renamedStates
          newEndStates = filter (not . null) (foldr (\state list -> (isStateGroupInEndStates state (endStates inputDKA)):list) [] renamedStates)
          newTransitions = getNewTransitions renamedStates (eqClassGroupTransitions eqClass)

-- spravi redukovany DKA z povodneho
createReducedDKA :: DKA -> DKA
createReducedDKA inputDKA = convertEquivalenceClassToDKA inputDKA lastEquivalenceClass
    where zeroEquivalenceClass = EqClass {
              eqClassStateGroups = [(endStates inputDKA), ((states inputDKA) \\ (endStates inputDKA))],
              eqClassAlphabet = alphabet inputDKA,
              eqClassGroupTransitions = [],
              eqClassTempStateGroups = []
          }
          lastEquivalenceClass = reduceDKA zeroEquivalenceClass ((transitions inputDKA))

-- funkcia skonkatenuje dve polia stringov do jedneho
-- prebrana z https://softwareengineering.stackexchange.com/questions/173559/using-foldr-to-append-two-lists-together-haskell
append :: [String] -> [String] -> [String]
append xs ys = foldr (\x y -> x:y) ys xs

-- funkcia pre citanie standardneho vstupu
readFromStdin :: [String] -> IO [String]
readFromStdin lines = do
    line <- getLine  
    if null line  
        then return (append lines (line:[]))
        else do  
            readFromStdin (append lines (line:[]))

-- nacitanie stdin
handleStdin :: IO [String]
handleStdin = readFromStdin []

-- funkcia pre citanie obsahu suboru
handleFile :: String -> IO [String]
handleFile fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let input = lines contents
    return input


main = do 
    arguments <- getArgs
    let argLength = length arguments
    content <- case argLength of
                   1 -> handleStdin
                   2 -> handleFile (arguments!!1)
                   _ -> error "Invalid number of arguments."
    let clearContent = filter (not . null) content

    let parsedDKA = parseInput clearContent
    case head arguments of
        "-i" -> printDKA parsedDKA    
        "-t" -> printMinimizedDKA (createReducedDKA (createWellDefinedDKA parsedDKA)) 
        _ -> error "Invalid first argument."
    return ()
