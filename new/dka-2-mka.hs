-- FLP projekt
-- varianta: dka-2-mka
-- autor: Maros Vasilisin, xvasil02

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.List (sortBy)
import Data.Function (on)

-- ****************** DATOVE TYPY *****************************

-- vlastna reprezentacia prechodov
data Transition = Transition {
  from :: String,
  symbol :: String,
  to :: String
} deriving (Show, Eq, Ord)

-- vlastna reprezentacia DKA
data DKA = DKA {
  allStates :: [String],
  alphabet :: [String],
  transitions :: [Transition],
  startStates :: [String],
  endStates :: [String]
} deriving (Show)

-- reprezentacia prechodu v ekv. triede, ukladame si aj do ktorej skupiny patri koncovy stav
data EqTransition = EqTransition {
  transition :: Transition,
  endStateGroupId :: Int
} deriving (Show, Eq)

-- struktura pre jednu skupinu v eq triede, obsahuje id, zoznam stavov a prechodov
data EqGroup = EqGroup {
  groupId :: Int,
  stateList :: [String],
  transitionList :: [EqTransition]
} deriving (Show, Eq)

-- struktura pre eq triedu, obsahuje skupiny
data EqClass = EqClass {
  groups :: [EqGroup]
} deriving (Show, Eq)

-- reprezentacia sink stavu
sink = "sink"

-- ****************** NACITANIE VSTUPU *****************************

-- podla poctu argumentov vrati stdin alebo otvoreny subor
getHandle :: [String] -> IO Handle
getHandle args = 
  if (length args == 1)
    then return stdin
    else openFile (args!!1) ReadMode

-- nacitanie vstupu zo stdin (http://learnyouahaskell.com/input-and-output)
readFromStdin :: Handle -> String -> IO String
readFromStdin handle input = 
  do
    line <- hGetLine handle
    if null line
      then return (input ++ line)
      else readFromStdin handle (input ++ line ++ "\n")

-- podla poctu argumentov nacitanie zo stdin alebo zo suboru
getContentsFromInput :: Handle -> [a] -> IO String
getContentsFromInput handle args = 
  if (length args == 1)
    then readFromStdin handle ""
    else hGetContents handle

-- ****************** FORMATOVANIE VSTUPU *****************************

-- funkcia na rozdelenie stringu na znaku
-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
customSplit :: Eq a => a -> [a] -> [[a]]
customSplit d [] = []
customSplit d s = x : customSplit d (drop 1 y) where (x,y) = span (/= d) s

-- z abecedy v jednom stringu spravi list
parseAlphabet :: [String] -> [String]
parseAlphabet [] = []
parseAlphabet [x] = [(customSplit ',' x)!!1] 
parseAlphabet (x:xs) = [(customSplit ',' x)!!1] ++ parseAlphabet xs 

-- z prechodu v stringu spravi objekt
parseTransition :: [String] -> Transition
parseTransition x = Transition {
  from = x!!0,
  symbol = x!!1,
  to = x!!2
}

-- zaradom prechadza vsetky prechody a po jednom ich parsuje
parseTransitionList :: [String] -> [Transition]
parseTransitionList [] = []
parseTransitionList [x] = [parseTransition (customSplit ',' x)]
parseTransitionList (x:xs) = [parseTransition (customSplit ',' x)] ++ parseTransitionList xs

-- zo vstupu vo forme stringu vytvori objekt DKA
formatInput :: [String] -> DKA
formatInput input = DKA {
  allStates = (customSplit ',' (input!!0)),
  alphabet = (sort (nub (parseAlphabet (drop 3 input)))),
  transitions = parseTransitionList (drop 3 input),
  startStates = (customSplit ',' (input!!1)),
  endStates = (customSplit ',' (input!!2))
}

-- ****************** VYPIS *****************************

printTransition :: Transition -> IO ()
printTransition transition = do
  putStr (from transition)
  putStr ","
  putStr (symbol transition)
  putStr ","
  putStrLn (to transition) 

-- prepinac -i
printCustomDKA :: DKA -> IO ()
printCustomDKA dka = do
  putStrLn "Stavy: "
  putStrLn (intercalate "," (allStates dka))
  putStrLn "Abeceda: "
  putStrLn (intercalate "," (alphabet dka))
  putStrLn "Prechody: "
  mapM_ printTransition (transitions dka)
  putStrLn "Startovaci stav: "
  putStrLn (intercalate "," (startStates dka))
  putStrLn "Koncove stavy: "
  putStrLn (intercalate "," (endStates dka))
  
-- prepinac -t
printDKA :: DKA -> IO ()
printDKA dka = do
  putStrLn (intercalate "," (allStates dka))
  putStrLn (intercalate "," (startStates dka))
  putStrLn (intercalate "," (endStates dka))
  mapM_ printTransition (transitions dka)  

-- ****************** ELIMINACIA NEDOSIAHNUTELNYCH STAVOV ***********

-- pre dany stav x najde vsetky stavy do ktorych su prechody
getReachableStatesFromOneState :: [String] -> [Transition] -> [String]
getReachableStatesFromOneState [x] transitions = 
  foldr (\transition reachableStates -> 
    if (from transition) `elem` reachableStates
      then [to transition] ++ reachableStates
      else reachableStates) [x] transitions

-- cyklus - pre kazdy stav z Si najde vsetky stavy do ktorych sa vie dostat
getReachableStatesFromSi :: [String] -> [Transition] -> [String]
getReachableStatesFromSi [] _ = []
getReachableStatesFromSi (x:xs) transitions = (getReachableStatesFromOneState [x] transitions) ++ getReachableStatesFromSi xs transitions

-- porovna zoznamy Si a Sii
compareReachableStateLists :: [String] -> [String] -> [Transition] -> [String]
compareReachableStateLists si sii transitions =
  if (si == sii)
    then sii
    else compareReachableStateLists sii (sort (nub (getReachableStatesFromSi sii transitions))) transitions

-- podla algoritmu z TIN, do Si vlozime pociatocne stavy,
-- do Sii vlozime stavy do ktorych sa vieme dostat z pociatocnych
-- opakujeme dokym Si != Sii
createReachableStates :: [String] -> [Transition] -> [String]
createReachableStates startStates transitions = 
  let si = startStates
      sii = sort (nub (getReachableStatesFromSi si transitions))
  in compareReachableStateLists si sii transitions

-- vyfiltruje koncove stavy zo zoznamu vsetkych dostupnych stavov
createReachableEndStates :: [String] -> [String] -> [String]
createReachableEndStates reachables endStates = sort (intersect reachables endStates)

-- -- ****************** ELIMINACIA NEDOSIAHNUTELNYCH PRECHODOV **********

-- zmaze tie prechody ktore vedu do nedostupnych stavov, alebo z nich vychadzaju
createReachableTransitions :: [Transition] -> [String] -> [Transition]
createReachableTransitions _ [] = []
createReachableTransitions [] _ = []
createReachableTransitions (x:xs) allStates =
  if ((from x) `elem` allStates) && ((to x) `elem` allStates)
    then [x] ++ createReachableTransitions xs allStates
    else createReachableTransitions xs allStates

-- -- ****************** PRECHODY DO SINK *****************************

-- vlozi prechod zo "sink" do "sink"
createSinkTransition :: String -> String -> Transition
createSinkTransition state symbolFromAlphabet = 
  Transition {
    from = state,
    symbol = symbolFromAlphabet,
    to = sink
  }

-- pre dany stav a symbol vlozi prechody do "sink" stavu
getSinkTransitionsForStateAndSymbol :: String -> [Transition] -> String -> [Transition]
getSinkTransitionsForStateAndSymbol state [] symbolFromAlphabet = [createSinkTransition state symbolFromAlphabet]
getSinkTransitionsForStateAndSymbol state (x:xs) symbolFromAlphabet =
  if (state == (from x)) && (symbolFromAlphabet == (symbol x))
    then []
    else getSinkTransitionsForStateAndSymbol state xs symbolFromAlphabet

-- cyklus pre jeden stav a vsetky symboly na pridanie prechodov do "sink" stavu
getSinkTransitionsForState :: String -> [Transition] -> [String] -> [Transition]
getSinkTransitionsForState state transitions [] = []
getSinkTransitionsForState state transitions (x:xs) = getSinkTransitionsForStateAndSymbol state transitions x ++ getSinkTransitionsForState state transitions xs

-- cyklus cez vsetky stavy, prida prechody do "sink" stavu aby bol automat kompletny
addSinkTransitions :: [String] -> [Transition] -> [String] -> [Transition]
addSinkTransitions [] transitions alphabet = []
addSinkTransitions (x:xs) transitions alphabet = getSinkTransitionsForState x transitions alphabet ++ addSinkTransitions xs transitions alphabet

-- ak boli vlozene nejake prechody do "sink" stavu, tak ho pridame aj do zoznamu stavov
addSinkState :: Int -> [String]
addSinkState sinkTransitions = 
  if sinkTransitions > 0
    then [sink]
    else []

-- vlozi prechody "sink" do "sink"
addSinkToSinkTransitions :: [String] -> [Transition]
addSinkToSinkTransitions [] = []
addSinkToSinkTransitions (x:xs) = [createSinkTransition sink x] ++ (addSinkToSinkTransitions xs)


-- ****************** UPLNY AUTOMAT *****************************

createFullyDefinedDKA :: DKA -> DKA
createFullyDefinedDKA dka = 
  let reachableStates = createReachableStates (startStates dka) (transitions dka)
      reachableTransitions = createReachableTransitions (transitions dka) reachableStates
      sinkTransitions = addSinkTransitions reachableStates reachableTransitions (alphabet dka)
      sinkToSinkTransitions = addSinkToSinkTransitions (alphabet dka)
  in DKA {
      allStates = reachableStates ++ (addSinkState (length sinkTransitions)), -- vsetky stavy + sink ak bol potrebny
      startStates = startStates dka,
      endStates = createReachableEndStates reachableStates (endStates dka),
      transitions = reachableTransitions ++ sinkTransitions ++ sinkToSinkTransitions,
      alphabet = alphabet dka
    }

-- ***************** EKVIVALENCNA TRIEDA 0 ***************************

-- ziska zoznam vsetkych vstupnych stavov zo zoznamu prechodov
getStateListFromTransitions :: [EqTransition] -> [String]
getStateListFromTransitions [] = []
getStateListFromTransitions (x:xs) = [from (transition x)] ++ getStateListFromTransitions xs

-- vytvori jednu skupinu v ekv. triede, podla parametrov id a prechodov, stavy si vypocita
createEqGroup :: Int -> [EqTransition] -> EqGroup
createEqGroup id transitions = 
  EqGroup {
    groupId = id,
    stateList = sort (nub (getStateListFromTransitions transitions)),
    transitionList = transitions
  }

-- vytvori EqTransition z Transition podla toho ci ide do koncoveho stavu alebo nie
createEqTransition :: Transition -> DKA -> EqTransition
createEqTransition trans dka = 
  if (to trans) `elem` (endStates dka)
    then 
      EqTransition {
        transition = trans,
        endStateGroupId = 1
      }
    else
      EqTransition {
        transition = trans,
        endStateGroupId = 2
      } 

-- prechadza zoznam prechodov s danym vstupnym stavom, konvertuje ich na objekty EqTransition
createEqTransitionList :: String -> DKA -> [EqTransition]
createEqTransitionList state dka = 
  foldr (\transition eqTrans -> 
    if (from transition) == state
      then [createEqTransition transition dka] ++ eqTrans
      else eqTrans)
  [] (transitions dka)

-- ak je stav koncovy stav tak vratim jeho prechody
getEndEqTransitions :: DKA -> [EqTransition]
getEndEqTransitions dka =
  foldr (\state eqStates -> 
    if (state) `elem` (endStates dka)
      then (createEqTransitionList state dka) ++ eqStates
      else eqStates) 
  [] (allStates dka)

-- ak je stav nekoncovy tak vratim jeho prechody
getNotEndEqTransitions :: DKA -> [EqTransition]
getNotEndEqTransitions dka =
  foldr (\state eqStates -> 
    if (state) `elem` (endStates dka)
      then eqStates
      else (createEqTransitionList state dka) ++ eqStates) 
  [] (allStates dka)

-- vytvori dve ekv. skupiny podla toho ci to su prechody z koncovych alebo nekoncovych stavov
createEqGroupsFromTuples0 :: DKA -> [EqGroup]
createEqGroupsFromTuples0 dka = 
  let endGroup = createEqGroup 1 (getEndEqTransitions dka)
      notEndGroup = createEqGroup 2 (getNotEndEqTransitions dka)
  in [endGroup, notEndGroup]

-- ekvivalencna trieda 0 - ma dve skupiny stavov: 1 su koncove a 2 nekoncove    
createEqClass0 :: DKA -> EqClass
createEqClass0 dka = 
  EqClass {
    groups = createEqGroupsFromTuples0 dka
  }

-- ********************* MAPA STAVOV V SKUPINACH ************************

-- vytvori zoznam skupin do ktorych vedu prechody
createGroupMapEntry :: [EqTransition] -> [Int]
createGroupMapEntry [] = []
createGroupMapEntry (x:xs) = [endStateGroupId x] ++ createGroupMapEntry xs

-- najde v mape stavov zoznam
getGroupMapEntry :: [(Int, [Int])] -> Int -> [Int]
getGroupMapEntry [] _ = []
getGroupMapEntry (x:xs) name =
  if (fst x) == name
    then snd x
    else getGroupMapEntry xs name

-- pre kazdu skupinu vrati zoznam skupin do ktorych vedu prechody v danej skupine
getGroupMap :: [EqGroup] -> [(Int, [Int])]
getGroupMap [] = []
getGroupMap (x:xs) = [(groupId x, sort (nub (createGroupMapEntry (transitionList x))))] ++ getGroupMap xs

-- vrati zoznam vstupnych stavov pre dany zoznam prechodov
getStatesFromTransitionList :: [EqTransition] -> [String]
getStatesFromTransitionList [] = []
getStatesFromTransitionList (x:xs) = [(from (transition x))] ++ getStatesFromTransitionList xs

-- cyklus na zistenie vstupnych stavov
getStatesFromTransitionLists :: [[EqTransition]] -> [String]
getStatesFromTransitionLists [] = []
getStatesFromTransitionLists (x:xs) = getStatesFromTransitionList x ++ getStatesFromTransitionLists xs

-- vytvori novy zoznam stavov v kazdej skupine po kroku minimalizacie
createNewGroupMap :: [(Int, [[EqTransition]])] -> [(Int, [String])]
createNewGroupMap [] = []
createNewGroupMap (x:xs) = [((fst x), sort (nub (getStatesFromTransitionLists(snd x))))] ++ createNewGroupMap xs

-- ******************** ROZDELOVANIE SKUPIN **************************

-- skupiny v ekv. triede skonvertuje na tuples, je to priprava na rozdelovanie skupin na mensie
convertGroupsToTuples :: [EqGroup] -> [(Int, [[EqTransition]])]
convertGroupsToTuples [] = []
convertGroupsToTuples (x:xs) = [((groupId x), [(transitionList x)])] ++ convertGroupsToTuples xs

-- pre jednu skupinu prechodov spocita kolko ich je cez dany symbol
countTransitionsForSymbol :: [EqTransition] -> String -> Int
countTransitionsForSymbol [] _ = 0
countTransitionsForSymbol (x:xs) sym = 
  if (symbol (transition x)) == sym
    then 1 + countTransitionsForSymbol xs sym
    else countTransitionsForSymbol xs sym

-- spocita kolko prechodov cez dany symbol je v kazdej skupine prechodov
countTransitionListForSymbol :: [[EqTransition]] -> String -> [Int]
countTransitionListForSymbol [] _ = []
countTransitionListForSymbol (x:xs) sym = [countTransitionsForSymbol x sym] ++ countTransitionListForSymbol xs sym

-- v zozname prechodov najde tie s danym symbolom a konecnym stavom, vrati pociatocne stavy
getStatesFromEqTransitionList :: Int -> [EqTransition] -> String -> [String]
getStatesFromEqTransitionList _ [] _ = []
getStatesFromEqTransitionList groupNumber (x:xs) sym = 
  if ((symbol (transition x)) == sym) && ((endStateGroupId x) == groupNumber)
    then [from (transition x)] ++ getStatesFromEqTransitionList groupNumber xs sym
    else getStatesFromEqTransitionList groupNumber xs sym

-- pre zoznam stavov najde vsetky prechody z danych stavov
getTransitionListForStates :: [String] -> [EqTransition] -> [EqTransition]
getTransitionListForStates [] _ = []
getTransitionListForStates _ [] = []
getTransitionListForStates allStates (x:xs) =
  if (from (transition x) `elem` allStates)
    then [x] ++ getTransitionListForStates allStates xs
    else getTransitionListForStates allStates xs

-- pre kazdu skupinu konecnych stavov v danej skupine najde vsetky prechody do danej skupiny,
-- vytvori z nich zoznam prechodov a taketo zoznamy da do jedneho zoznamu a vrati
splitTransitionsForGroups :: [Int] -> [EqTransition] -> String -> [[EqTransition]]
splitTransitionsForGroups [] _ _ = []
splitTransitionsForGroups _ [] _ = []
splitTransitionsForGroups (x:xs) trans sym =
  let states = getStatesFromEqTransitionList x trans sym
      transitionList = getTransitionListForStates states trans
  in [transitionList] ++ splitTransitionsForGroups xs trans sym

-- ak v danej skupine pre dany symbol je viac ako jedna koncova skupina, tak sa musi rozdelit
-- ak nie je tak sa len aktualna skupina ulozi a pokracuje sa na dalsiu
createNewTransitions :: [(Int, [EqTransition])] -> [Int] -> String -> [[EqTransition]]
createNewTransitions [] _ _ = []
createNewTransitions  _ [] _ = []
createNewTransitions (x:xs) groupMapEntry sym =
  if (fst x) > 1
    then splitTransitionsForGroups groupMapEntry (snd x) sym ++ createNewTransitions xs groupMapEntry sym
    else [(snd x)] ++ createNewTransitions xs groupMapEntry sym

-- vezme aktualne skupiny prechodov, zaradom prechadza symboly abecedy a rozdeluje skupiny
createNewEqTransitionLists :: [[EqTransition]] -> [String] -> [Int] -> [[EqTransition]]
createNewEqTransitionLists [] _ _ = []
createNewEqTransitionLists x [] _ = x
createNewEqTransitionLists _ _ [] = []
createNewEqTransitionLists trans (x:xs) groupMapEntry = 
  let counts = countTransitionListForSymbol trans x
      zippedLists = zip counts trans
      newTransitions = createNewTransitions zippedLists groupMapEntry x
  in createNewEqTransitionLists newTransitions xs groupMapEntry

-- dostane aktualne dvojice (nazov skupiny, jej prechody), abecedu a aktualnu mapu stavov
-- najde si do ktorych stavov sa vie v danej skupine dostat, vypocita nove skupiny a vrati
splitTuples :: [(Int, [[EqTransition]])] -> [String] -> [(Int, [Int])] -> [(Int, [[EqTransition]])]
splitTuples [] _ _ = []
splitTuples _ [] _ = []
splitTuples _ _ [] = []
splitTuples (x:xs) alphabet groupMap = 
  let entry = getGroupMapEntry groupMap (fst x)
      newTransitionLists = filter (not . null) (createNewEqTransitionLists (snd x) alphabet entry)
  in [((fst x), newTransitionLists)] ++ splitTuples xs alphabet groupMap

-- ************************* PREMENOVANIE SKUPIN *********************

renameOneTuple :: Int -> [[EqTransition]] -> [(Int, [[EqTransition]])]
renameOneTuple _ [] = []
renameOneTuple i (x:xs) = [(i, [x])] ++ renameOneTuple (i+1) xs

renameTuples :: [(Int, [[EqTransition]])] -> Int -> [(Int, [[EqTransition]])]
renameTuples [] _ = [] 
renameTuples (x:xs) i = 
  let len = (length (snd x)) + i
  in renameOneTuple i (snd x) ++ renameTuples xs len

-- ********************* OPRAVA KONCOVYCH STAVOV *********************************

-- vrati nove group id podla toho do ktorej skupiny patri stav po novom
getGroupId :: [(Int, [String])] -> String -> Int
getGroupId [] _ = 0
getGroupId (x:xs) state = 
  if state `elem` (snd x)
    then fst x
    else getGroupId xs state

-- vzdy zisti spravnu skupinu a opravi group id, vrati prechod
fixEndStateGroupId :: [EqTransition] -> [(Int, [String])] -> [EqTransition]
fixEndStateGroupId [] _ = []
fixEndStateGroupId _ [] = []
fixEndStateGroupId (x:xs) groupMap = 
  let fixedEndGroup = getGroupId groupMap (to (transition x))
      fixedTransition = EqTransition {
        transition = transition x,
        endStateGroupId = fixedEndGroup
      }   
  in [fixedTransition] ++ fixEndStateGroupId xs groupMap 

-- vrati zoznam prechodov s opravenymi koncovymi stavmi
getTransitionsWithFixedEndGroup :: [[EqTransition]] -> [(Int, [String])] -> [[EqTransition]]
getTransitionsWithFixedEndGroup [] _ = []
getTransitionsWithFixedEndGroup _ [] = []
getTransitionsWithFixedEndGroup (x:xs) groupMap = [fixEndStateGroupId x groupMap] ++ getTransitionsWithFixedEndGroup xs groupMap 

-- opravi id koncovych stavov pre vsetky skupiny
fixEndGroups :: [(Int, [[EqTransition]])] -> [(Int, [String])] -> [(Int, [[EqTransition]])]
fixEndGroups [] _ = []
fixEndGroups _ [] = []
fixEndGroups (x:xs) groupMap = [((fst x), getTransitionsWithFixedEndGroup (snd x) groupMap)] ++ fixEndGroups xs groupMap

-- ******************** NOVA EKVIVALENCNA TRIEDA ************************

-- vrati zoznam stavov pre danu skupinu
getStatesInGroup :: Int -> [(Int, [String])] -> [String]
getStatesInGroup _ [] = []
getStatesInGroup groupNumber (x:xs) =
  if groupNumber == (fst x)
    then snd x
    else getStatesInGroup groupNumber xs

-- z vypoctov vytvori novy zoznam skupin v danej ekv. triede
createEqGroupsFromTuples :: [(Int, [[EqTransition]])] -> [(Int, [String])] -> [EqGroup]
createEqGroupsFromTuples [] _ = []
createEqGroupsFromTuples _ [] = []
createEqGroupsFromTuples (x:xs) groupMap = 
  [
    EqGroup {
        groupId = fst x,
        transitionList = concat (snd x),
        stateList = getStatesInGroup (fst x) groupMap
    }
  ] ++ createEqGroupsFromTuples xs groupMap

-- vytvori novu ekvivalencnu triedu z vypoctov jedneho kroku minimalizacie
createNewEqClass :: [(Int, [[EqTransition]])] -> [(Int, [String])] -> EqClass
createNewEqClass tuples groupMap = 
  EqClass {
    groups = createEqGroupsFromTuples tuples groupMap
  } 

-- ******************** KONVERZIA NA DKA ****************************

-- pre zoznam skupin vrati ich idcka
getGroupNumbers :: [EqGroup] -> [String]
getGroupNumbers [] = []
getGroupNumbers (x:xs) = [show (groupId x)] ++ getGroupNumbers xs

-- ak sa stav nachadza v danej skupine, tak vrati jej id
findGroup :: String -> [EqGroup] -> String
findGroup _ [] = []
findGroup state (x:xs) =
  if state `elem` (stateList x)
    then show (groupId x)
    else findGroup state xs

-- zisti v ktorej skupine su stavy
getGroups :: [EqGroup] -> [String] -> [String]
getGroups [] _ = []
getGroups _ [] = []
getGroups groups (x:xs) = [findGroup x groups] ++ getGroups groups xs

-- zoznam EqTransition skonvertuje na zoznam Transition
convertEqTransitionListToTransitionsList :: Int -> [EqTransition] -> [Transition]
convertEqTransitionListToTransitionsList _ [] = []
convertEqTransitionListToTransitionsList num (x:xs) = 
  [
    Transition {
      from = show num,
      symbol = symbol (transition x),
      to = show (endStateGroupId x)
    }
  ] ++ convertEqTransitionListToTransitionsList num xs


-- vrati vsetky prechody danej EqClass
getNewTransitions :: [EqGroup] -> [Transition]
getNewTransitions [] = []
getNewTransitions (x:xs) = convertEqTransitionListToTransitionsList (groupId x) (transitionList x) ++ getNewTransitions xs


-- skonvertuje EqClass pouzivanu na vypocty na DKA ktory sa potom vypise
convertEqClassToDKA :: EqClass -> DKA -> DKA
convertEqClassToDKA eqClass oldDKA =
  let newStates = sort (nub (getGroupNumbers (groups eqClass)))
      newStart = sort (nub (getGroups (groups eqClass) (startStates oldDKA)))
      newEnd = sort (nub (getGroups (groups eqClass) (endStates oldDKA)))
      newTransitions = sort (nub (getNewTransitions (groups eqClass)))
  in DKA {
    allStates = newStates,
    alphabet = alphabet oldDKA,
    startStates = newStart,
    endStates = newEnd,
    transitions = newTransitions
  }  

-- ****************** PREMENOVANIE PRED VYPISOM ***********************

-- funkcia zoradi list dvojic podla fst
-- https://stackoverflow.com/questions/30380697/sort-tuples-by-one-of-their-elements-in-haskell
customTupleSort :: Ord a => [(a, b)] -> [(a, b)]
customTupleSort = sortBy (compare `on` fst)

-- vytvori zoznam dvojic aby sme vedeli ktore stavy su v ktorej skupine
generateTuple :: EqGroup -> [(String, Int)]
generateTuple group = 
  foldr (\state res ->
    [(state, (groupId group))] ++ res) 
  [] (stateList group)

-- loop cez vsetky skupiny
generateTuples :: [EqGroup] -> [(String, Int)]
generateTuples [] = []
generateTuples (x:xs) = generateTuple x ++ generateTuples xs 

-- vytvori zoznam dvojic, ktory urcuje ako budeme premenovat prechody
createConversionList :: [(String, Int)] -> [Int] -> Int -> [(Int, Int)]
createConversionList [] _ _ = []
createConversionList (x:xs) used cnt = 
  if ((snd x) `elem` used)
    then createConversionList xs used cnt
    else [((snd x), cnt)] ++ createConversionList xs (used ++ [snd x]) (cnt + 1)

-- premenuje prechody
createRenamedEqTransitions :: [EqTransition] -> [(Int, Int)] -> [EqTransition]
createRenamedEqTransitions [] _ = []
createRenamedEqTransitions _ [] = []
createRenamedEqTransitions (x:xs) conversionList =
  let maybeNewId = lookup (endStateGroupId x) conversionList
  in 
    case maybeNewId of
        Just n  ->  [
                      EqTransition {
                        transition = transition x,
                        endStateGroupId = n
                      }
                    ] ++ createRenamedEqTransitions xs conversionList
        Nothing -> error ("Chyba v prevode")

-- premenuje cisla skupin, tym padom cisla stavov
createRenamedEqGroups :: [EqGroup] -> [(Int, Int)] -> [EqGroup]
createRenamedEqGroups [] _ = []
createRenamedEqGroups _ [] = []
createRenamedEqGroups (x:xs) conversionList = 
  let maybeNewId = lookup (groupId x) conversionList
  in 
    case maybeNewId of
        Just n  ->  [
                      EqGroup {
                        groupId = n,
                        stateList = stateList x,
                        transitionList = createRenamedEqTransitions (transitionList x) conversionList
                      }
                    ] ++ createRenamedEqGroups xs conversionList
        Nothing -> error ("Chyba v prevode")

-- najde prvok podla prveho parametra v dvojici
findTuple :: String -> [(String, Int)] -> (String, Int)
findTuple s (x:xs) =
  if s == (fst x)
    then x
    else findTuple s xs

-- zmaze prvok podla prveho parametra v dvojici
deleteTuple :: String -> [(String, Int)] -> [(String, Int)]
deleteTuple s v@(x:xs) =
  if s == (fst x)
    then xs
    else x : deleteTuple s xs 

-- spravi to aby startovaci stav v povodnom automate bol vzdy v skupine 0
moveStartState :: [(String, Int)] -> [String] -> [(String, Int)]
moveStartState [] _ = []
moveStartState _ [] = []
moveStartState list (s:ss) =
  let elem = findTuple s list
      rest = deleteTuple s list
  in elem : rest

-- konvertuje EqClass tak aby mala spravne premenovane stavy a prechody
createEqClassWithRenamedStates :: EqClass -> [String] -> EqClass
createEqClassWithRenamedStates eqClass [] = eqClass
createEqClassWithRenamedStates eqClass dkaStartStates =
  let tuples = generateTuples (groups eqClass)
      sortedTuples = customTupleSort tuples
      sortedWithStart = moveStartState sortedTuples dkaStartStates
      conversionList = createConversionList sortedWithStart [] 0
  in 
    EqClass {
      groups = createRenamedEqGroups (groups eqClass) conversionList
    }

-- ************************** MINIMALNY AUTOMAT *********************

-- algoritmus minimalizacie z TIN, do i si ulozi aktualnu situaciu
-- do ii potom zmenene prechody po 1 kroku algoritmu, kde sa skupiny rozdelia
-- skupiny sa nasledne premenuju, opravia sa prechody, vytvori sa nova ekvivalencna trieda
-- povodna a nova ekvivalencna trieda sa porovnaju, opakujeme pokial sa nebudu zhodovat
createMinimalEqClass :: EqClass -> [String] -> EqClass
createMinimalEqClass eqClass alphabet = 
  let groupMap = getGroupMap (groups eqClass) 
      i = convertGroupsToTuples (groups eqClass)
      ii = splitTuples i alphabet groupMap
      renamedTuples = renameTuples ii 0
      newGroupMap = createNewGroupMap renamedTuples
      fixedEndGroups = fixEndGroups renamedTuples newGroupMap
      newEqClass = createNewEqClass fixedEndGroups newGroupMap
  in 
    if (newEqClass == eqClass)
      then newEqClass
      else createMinimalEqClass newEqClass alphabet 

-- vypocita nulovu ekv. triedu, potom cyklom tu poslednu, a z nej vytvori minimalny DKA
createMinimalDKA :: DKA -> DKA
createMinimalDKA oldDKA = 
  let eqClass0 = createEqClass0 oldDKA
      eqClassMin = createMinimalEqClass eqClass0 (alphabet oldDKA)
      renamedEqClass = createEqClassWithRenamedStates eqClassMin (startStates oldDKA)
  in convertEqClassToDKA renamedEqClass oldDKA    

-- ****************************** KONTROLA VSTUPU *********************
checkState :: String -> String
checkState [] = []
checkState [x] =
  if isDigit x
    then [x]
    else []
checkState (x:xs) = 
  if isDigit x
    then [x] ++ checkState xs
    else []    

checkInput :: DKA -> (Bool, DKA)
checkInput dka =
  let 
    correctAlphabet = foldr (\symbol symbols -> 
      if (length symbol) == 1
        then [symbol] ++ symbols
        else symbols) [] (alphabet dka)
    correctStates = foldr (\state states -> 
      if length [checkState state] < length state
        then states
        else [checkState state] ++ states
      ) [] (allStates dka)
    correctStartStates = foldr (\state states -> 
      if length [checkState state] < length state
        then states
        else [checkState state] ++ states
      ) [] (startStates dka)
    correctEndStates = foldr (\state states -> 
      if length [checkState state] < length state
        then states
        else [checkState state] ++ states
      ) [] (endStates dka)

    newDKA = DKA {
      allStates = correctStates,
      startStates = correctStartStates,
      endStates = correctEndStates,
      transitions = transitions dka,
      alphabet = correctAlphabet
    }
  in 
    if ((length correctAlphabet) < (length (alphabet dka))) ||
      ((length correctStates) < (length (allStates dka))) || 
      (length (allStates dka)) < 1 ||
      (length (allStates dka)) < (length (startStates dka)) ||
      (length (allStates dka)) < (length (endStates dka))
      then 
        (False, newDKA)
      else 
        (True, newDKA)  

minimize :: DKA -> String -> IO ()
minimize dka switcher = 
  case switcher of 
    "-i" -> printCustomDKA dka
    "-t" -> printDKA (createMinimalDKA (createFullyDefinedDKA dka))
    _ -> error "Chybny prepinac"

-- ******************************** MAIN *****************************
main = do
    args <- getArgs
    let argCount = length args
    if (argCount == 0 || argCount > 2)
      then error "Chybny pocet argumentov"
      else do
        handle <- getHandle args
        contents <- getContentsFromInput handle args

        let formattedInput = formatInput (lines contents)
        let checkedInput = checkInput formattedInput
        
        let switcher = args!!0
        -- putStrLn (show (fst checkedInput))
        -- printCustomDKA (snd checkedInput)
        if (fst checkedInput) == True
          then 
            minimize (snd checkedInput) switcher
          else 
            error "Chybny vstup"    
        hClose handle

    return ()