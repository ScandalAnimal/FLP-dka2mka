-- FLP projekt
-- varianta: dka-2-mka
-- autor: Maros Vasilisin, xvasil02

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Char

-- ****************** CORRECT *****************************

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

data EqTransition = EqTransition {
  transition :: Transition,
  endStateGroupId :: Int
} deriving (Show, Eq)

-- struktura pre jednu skupinu v eq triede
data EqGroup = EqGroup {
  groupId :: Int,
  stateList :: [String],
  transitionList :: [EqTransition]
} deriving (Show, Eq)

-- struktura pre eq triedu
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
createReachableEndStates reachables endStatess = sort (intersect reachables endStatess)

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

-- ****************** WIP *****************************

data Test = Test {
  eqClass :: EqClass,
  trans :: [(Int, [[EqTransition]])],
  trans2 :: [(Int, [[EqTransition]])],
  groupCount :: Int,
  groupStateList :: [(Int, [String])],
  zipped :: [(Int, [EqTransition])]
} deriving (Show)


printEqTrans :: EqTransition -> IO ()
printEqTrans eq = do
  putStrLn ("    " ++ (from (transition eq)) ++ "," 
    ++ (symbol (transition eq)) ++ "," ++ (to (transition eq)) ++ "("
    ++ (show (endStateGroupId eq)) ++ ")")

printEqGroup :: EqGroup -> IO ()
printEqGroup eq = do
  putStrLn ("group " ++ (show (groupId eq)))
  putStrLn (intercalate "," (stateList eq))
  mapM_ printEqTrans (transitionList eq)

printEqTrans2 :: [EqTransition] -> IO ()
printEqTrans2 eq = do
  putStrLn ("skupina: ")
  mapM_ printEqTrans eq

printTestTrans :: (Int, [[EqTransition]]) -> IO ()
printTestTrans x = do
  putStrLn (show (fst x))
  mapM_ printEqTrans2 (snd x)
    
printGSL :: (Int, [String]) -> IO ()
printGSL x = do
  putStr ((show(fst x)) ++ ": ")
  putStr (intercalate "," (snd x))
  putStrLn ("")

printZipped :: (Int, [EqTransition]) -> IO ()
printZipped eq = do
  putStrLn (show (fst eq))
  mapM_ printEqTrans (snd eq)


printEqClass :: EqClass -> IO ()
printEqClass eq = do
  -- putStrLn ("GSL: ")
  -- mapM_ printGSL (groupStateList eq)
  putStrLn ("GROUPS: ")
  mapM_ printEqGroup (groups eq)
  -- putStrLn ("pocet skupin: " ++ show (groupCount eq))
  -- mapM_ printTestTrans (trans eq)
  -- putStrLn ("new")
  -- mapM_ printTestTrans (trans2 eq)
  -- putStrLn ("zipped")
  -- mapM_ printZipped (zipped eq)

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

createEqTransitions :: String -> DKA -> [EqTransition]
createEqTransitions state dka = 
  foldr (\transition eqTrans -> 
    if (from transition) == state
      then [createEqTransition transition dka] ++ eqTrans
      else eqTrans)
  [] (transitions dka)

getEndEqTransitions :: DKA -> [EqTransition]
getEndEqTransitions dka =
  foldr (\state eqStates -> 
    if (state) `elem` (endStates dka)
      then (createEqTransitions state dka) ++ eqStates
      else eqStates) 
  [] (allStates dka)

getNotEndEqTransitions :: DKA -> [EqTransition]
getNotEndEqTransitions dka =
  foldr (\state eqStates -> 
    if (state) `elem` (endStates dka)
      then eqStates
      else (createEqTransitions state dka) ++ eqStates) 
  [] (allStates dka)

getStateListFromTransitions :: [EqTransition] -> [String]
getStateListFromTransitions [] = []
getStateListFromTransitions (x:xs) = [from (transition x)] ++ getStateListFromTransitions xs

createEqGroup :: Int -> [EqTransition] -> EqGroup
createEqGroup num transitions = 
  let stateList = sort (nub (getStateListFromTransitions transitions))
  in
  EqGroup {
    groupId = num,
    stateList = stateList,
    transitionList = transitions
  }

createEqGroups0 :: DKA -> [EqGroup]
createEqGroups0 dka = 
  let endStateGroupId = createEqGroup 1 (getEndEqTransitions dka)
      notEndGroup = createEqGroup 2 (getNotEndEqTransitions dka)
  in [endStateGroupId, notEndGroup]

-- ekvivalencna trieda 0 - ma dve skupiny stavov: 1 su koncove a 2 nekoncove    
createEqClass0 :: DKA -> EqClass
createEqClass0 dka = 
  let eqGroups = createEqGroups0 dka
  in 
    EqClass {
      groups = eqGroups  
    }

getEndGroups :: [EqTransition] -> [Int]
getEndGroups [] = []
getEndGroups (x:xs) = [endStateGroupId x] ++ getEndGroups xs

checkEndGroups :: [EqGroup] -> [(Int, [Int])]
checkEndGroups [] = []
checkEndGroups (x:xs) = [(groupId x, sort (nub (getEndGroups (transitionList x))))] ++ checkEndGroups xs

getCurrentTransitions :: [EqGroup] -> [(Int, [[EqTransition]])]
getCurrentTransitions [] = []
getCurrentTransitions (x:xs) = [((groupId x), [(transitionList x)])] ++ getCurrentTransitions xs

createGroupStateList :: [EqGroup] -> [(Int, [String])]
createGroupStateList [] = []
createGroupStateList (x:xs) = [((groupId x), (stateList x))] ++ createGroupStateList xs

countTrans2 :: [EqTransition] -> String -> Int
countTrans2 [] _ = 0
countTrans2 (x:xs) sym = 
  if (symbol (transition x)) == sym
    then 1 + countTrans2 xs sym
    else countTrans2 xs sym

countTrans :: [[EqTransition]] -> String -> [Int]
countTrans [] _ = []
countTrans (x:xs) sym = [countTrans2 x sym] ++ countTrans xs sym

findAll :: Int -> [EqTransition] -> String -> [String]
findAll _ [] _ = []
findAll groupNumber (x:xs) sym = 
  if ((symbol (transition x)) == sym) && ((endStateGroupId x) == groupNumber)
    then [from (transition x)] ++ findAll groupNumber xs sym
    else findAll groupNumber xs sym

getTrans :: [String] -> [EqTransition] -> [EqTransition]
getTrans [] _ = []
getTrans _ [] = []
getTrans allStates (x:xs) =
  if (from (transition x) `elem` allStates)
    then [x] ++ getTrans allStates xs
    else getTrans allStates xs


countNew :: [Int] -> [EqTransition] -> String -> [[EqTransition]]
countNew [] _ _ = []
countNew _ [] _ = []
countNew (x:xs) trans sym =
  let a = findAll x trans sym
      getTransx = getTrans a trans
  in [getTransx] ++ countNew xs trans sym


newTrans :: [(Int, [EqTransition])] -> [Int] -> String -> [[EqTransition]]
newTrans [] _ _ = []
newTrans  _ [] _ = []
newTrans (x:xs) checkList sym =
  if (fst x) > 1
    then
      countNew checkList (snd x) sym ++ newTrans xs checkList sym
    else
      [(snd x)] ++ newTrans xs checkList sym

newTransitions2 :: [[EqTransition]] -> [String] -> [Int] -> [[EqTransition]]
-- newTransitions2 :: [[EqTransition]] -> [String] -> [String] -> [(Int, [EqTransition])]
newTransitions2 [] _ _ = []
newTransitions2 x [] _ = x
newTransitions2 _ _ [] = []
newTransitions2 trans (x:xs) groupCheckList = 
  let countTransx = countTrans trans x -- [1,4,5]
      zipped = zip countTransx trans
      newTransx = newTrans zipped groupCheckList x
      -- countTransxz = countTrans newTransx y -- [1,4,5]
      -- zippedx = zip countTransxz newTransx
      -- newTransxz = newTrans zippedx groupCheckList y
  in newTransitions2 newTransx xs groupCheckList
  -- in newTransxz
  -- in zipped

getGroupCheckList :: [(Int, [Int])] -> Int -> [Int]
getGroupCheckList [] _ = []
getGroupCheckList (x:xs) name =
  if (fst x) == name
    then snd x
    else getGroupCheckList xs name

newTransitions :: [(Int, [[EqTransition]])] -> [String] -> [(Int, [Int])] -> [(Int, [[EqTransition]])]
-- newTransitions :: [(String, [[EqTransition]])] -> [String] -> [(String, [String])] -> [(Int, [EqTransition])]
newTransitions [] _ _ = []
newTransitions _ [] _ = []
newTransitions _ _ [] = []
newTransitions (x:xs) alphabet endStateGroupIdCheck = 
  let groupCheckList = getGroupCheckList endStateGroupIdCheck (fst x) -- ["I","II"]
      a = filter (not . null) (newTransitions2 (snd x) alphabet groupCheckList)
  in [((fst x), a)] ++ newTransitions xs alphabet endStateGroupIdCheck
  -- in a ++ newTransitions xs alphabet endStateGroupIdCheck

renGroups :: Int -> [[EqTransition]] -> [(Int, [[EqTransition]])]
renGroups _ [] = []
renGroups i (x:xs) = [(i, [x])] ++ renGroups (i+1) xs

renameGroups :: [(Int, [[EqTransition]])] -> Int -> [(Int, [[EqTransition]])]
renameGroups [] _ = [] 
renameGroups (x:xs) i = 
  let len = (length (snd x)) + i
  in renGroups i (snd x) ++ renameGroups xs len

getStateNames2 :: [EqTransition] -> [String]
getStateNames2 [] = []
getStateNames2 (x:xs) = [(from (transition x))] ++ getStateNames2 xs

getStateNames :: [[EqTransition]] -> [String]
getStateNames [] = []
getStateNames (x:xs) = getStateNames2 x ++ getStateNames xs

createNewGSL :: [(Int, [[EqTransition]])] -> [(Int, [String])]
createNewGSL [] = []
createNewGSL (x:xs) = [((fst x), sort (nub (getStateNames (snd x))))] ++ createNewGSL xs

getNewEndGroup :: [(Int, [String])] -> String -> Int
getNewEndGroup [] _ = 0
getNewEndGroup (x:xs) state = 
  if state `elem` (snd x)
    then fst x
    else getNewEndGroup xs state

getNewTrans2 :: [EqTransition] -> [(Int, [String])] -> [EqTransition]
getNewTrans2 [] _ = []
getNewTrans2 _ [] = []
getNewTrans2 (x:xs) gsl = 
  let newEndGroup = getNewEndGroup gsl (to (transition x))
      a = EqTransition {
        transition = transition x,
        endStateGroupId = newEndGroup
      }   
  in [a] ++ getNewTrans2 xs gsl 

getNewTrans :: [[EqTransition]] -> [(Int, [String])] -> [[EqTransition]]
getNewTrans [] _ = []
getNewTrans _ [] = []
getNewTrans (x:xs) gsl = [getNewTrans2 x gsl] ++ getNewTrans xs gsl 


fixEndGroups :: [(Int, [[EqTransition]])] -> [(Int, [String])] -> [(Int, [[EqTransition]])]
fixEndGroups [] _ = []
fixEndGroups _ [] = []
fixEndGroups (x:xs) gsl = [((fst x), getNewTrans (snd x) gsl)] ++ fixEndGroups xs gsl

findStates :: Int -> [(Int, [String])] -> [String]
findStates _ [] = []
findStates groupNumber (x:xs) =
  if groupNumber == (fst x)
    then snd x
    else findStates groupNumber xs

createEqGroups :: [(Int, [[EqTransition]])] -> [(Int, [String])] -> [EqGroup]
createEqGroups [] _ = []
createEqGroups _ [] = []
createEqGroups (x:xs) gsl = 
  [
    EqGroup {
        groupId = fst x,
        transitionList = concat (snd x),
        stateList = findStates (fst x) gsl
    }
  ] ++ createEqGroups xs gsl

createNewEqClass :: [(Int, [[EqTransition]])] -> [(Int, [String])] -> EqClass
createNewEqClass groups gsl = 
  EqClass {
    groups = createEqGroups groups gsl
  } 

reduceEqClass :: EqClass -> [String] -> EqClass
reduceEqClass eqClassI alphabet = 
  let endStateGroupIdCheck = checkEndGroups (groups eqClassI) 
      groupCount = length endStateGroupIdCheck
      i = getCurrentTransitions (groups eqClassI)
      -- ii = splitTransitions (groups eqClassI) groupStateList alphabet
      ii = newTransitions i alphabet endStateGroupIdCheck
      renamedGroups = renameGroups ii 1
      newGSL = createNewGSL renamedGroups
      fixedEndGroups = fixEndGroups renamedGroups newGSL
      newEqClass = createNewEqClass fixedEndGroups newGSL
  in 
    if (newEqClass == eqClassI)
      then newEqClass
      else reduceEqClass newEqClass alphabet 
    -- Test {
      -- eqClass = newEqClass,
      -- trans = i,
      -- trans2 = fixedEndGroups,
      -- groupCount = groupCount,
      -- groupStateList = createGroupStateList (groups eqClassI),
      -- groupStateList = newGSL,
      -- zipped = []
    -- }

getGroupNumbers :: [EqGroup] -> [String]
getGroupNumbers [] = []
getGroupNumbers (x:xs) = [show (groupId x)] ++ getGroupNumbers xs

findGroup :: String -> [EqGroup] -> String
findGroup state (x:xs) =
  if state `elem` (stateList x)
    then show (groupId x)
    else findGroup state xs

getStartGroup :: [EqGroup] -> [String] -> [String]
getStartGroup [] _ = []
getStartGroup _ [] = []
getStartGroup groups (x:xs) = [findGroup x groups] ++ getStartGroup groups xs

iterateT :: Int -> [EqTransition] -> [Transition]
iterateT _ [] = []
iterateT num (x:xs) = 
  [
    Transition {
      from = show num,
      symbol = symbol (transition x),
      to = show (endStateGroupId x)
    }
  ] ++ iterateT num xs


getNewTransitions :: [EqGroup] -> [Transition]
getNewTransitions [] = []
getNewTransitions (x:xs) = iterateT (groupId x) (transitionList x) ++ getNewTransitions xs

convertEqClassToDKA :: EqClass -> DKA -> DKA
convertEqClassToDKA eqClass oldDKA =
  let newStates = getGroupNumbers (groups eqClass)
      newStart = sort (nub (getStartGroup (groups eqClass) (startStates oldDKA)))
      newEnd = sort (nub (getStartGroup (groups eqClass) (endStates oldDKA)))
      newTransitions = sort (nub (getNewTransitions (groups eqClass)))
  in DKA {
    allStates = newStates,
    alphabet = alphabet oldDKA,
    startStates = newStart,
    endStates = newEnd,
    transitions = newTransitions
  }
-- TODO fix
makeReducedDKA :: DKA -> DKA
makeReducedDKA dka = 
  let eqClass0 = createEqClass0 dka
      eqClassMax = reduceEqClass eqClass0 (alphabet dka)
  in convertEqClassToDKA eqClassMax dka    

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

        let switcher = args!!0
        case switcher of 
          "-i" -> printCustomDKA formattedInput
          "-t" -> printDKA (makeReducedDKA (createFullyDefinedDKA formattedInput))
          _ -> error "Chybny prepinac"
        hClose handle

    return ()