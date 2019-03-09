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
} deriving (Show, Eq)

-- vlastna reprezentacia DKA
data DKA = DKA {
  states :: [String],
  alphabet :: [String],
  transitions :: [Transition],
  start :: [String],
  end :: [String]
} deriving (Show)

-- struktura reprezentuje jeden stav v eq skupine
-- data EqState = EqState {
  -- name :: String,
  -- trans :: [(String, String, String)] -- prechod (symbol, koncovy stav, cislo skupiny koncoveho stavu)
-- } deriving (Show)

data EqTransition = EqTransition {
  transition :: Transition,
  endGroup :: Int
} deriving (Show, Eq)

-- struktura pre jednu skupinu v eq triede
data  EqGroup = EqGroup {
  number :: Int,
  stateList :: [String],
  transList :: [EqTransition]
} deriving (Show)

-- struktura pre eq triedu
data EqClass = EqClass {
  groups :: [EqGroup]
} deriving (Show)

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

parseAlphabet :: [String] -> [String]
parseAlphabet [x] = [(customSplit ',' x)!!1] 
parseAlphabet (x:xs) = [(customSplit ',' x)!!1] ++ parseAlphabet xs 

parseTransition :: [String] -> Transition
parseTransition x = Transition {
  from = x!!0,
  symbol = x!!1,
  to = x!!2
}

parseTransitions :: [String] -> [Transition]
parseTransitions [] = []
parseTransitions [x] = [parseTransition (customSplit ',' x)]
parseTransitions (x:xs) = [parseTransition (customSplit ',' x)] ++ parseTransitions xs

formatInput :: [String] -> DKA
formatInput input = DKA {
  states = (customSplit ',' (input!!0)),
  alphabet = (sort (nub (parseAlphabet (drop 3 input)))),
  transitions = parseTransitions (drop 3 input),
  start = (customSplit ',' (input!!1)),
  end = (customSplit ',' (input!!2))
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
  putStrLn (intercalate "," (states dka))
  putStrLn "Abeceda: "
  putStrLn (intercalate "," (alphabet dka))
  putStrLn "Prechody: "
  mapM_ printTransition (transitions dka)
  putStrLn "Startovaci stav: "
  putStrLn (intercalate "," (start dka))
  putStrLn "Koncove stavy: "
  putStrLn (intercalate "," (end dka))
  
-- prepinac -t
printDKA :: DKA -> IO ()
printDKA dka = do
  putStrLn (intercalate "," (states dka))
  putStrLn (intercalate "," (start dka))
  putStrLn (intercalate "," (end dka))
  mapM_ printTransition (transitions dka)  

-- ****************** ELIMINACIA NEDOSIAHNUTELNYCH STAVOV ***********

getReachableStatesFromOneState :: [String] -> [Transition] -> [String]
getReachableStatesFromOneState [x] transitions = 
  foldr (\transition reachableStates -> 
    if (from transition) `elem` reachableStates
      then [to transition] ++ reachableStates
      else reachableStates) [x] transitions

compareReachableStateGroups :: [String] -> [String] -> [Transition] -> [String]
compareReachableStateGroups si sii transitions =
  if (si == sii)
    then sii
    else compareReachableStateGroups sii (sort (nub (getReachableStatesFromSi sii transitions))) transitions

getReachableStatesFromSi :: [String] -> [Transition] -> [String]
getReachableStatesFromSi [] _ = []
getReachableStatesFromSi [x] transitions = getReachableStatesFromOneState [x] transitions
getReachableStatesFromSi (x:xs) transitions = (getReachableStatesFromOneState [x] transitions) ++ getReachableStatesFromSi xs transitions

createReachableStates :: [String] -> [Transition] -> [String]
createReachableStates start transitions = 
  let si = start
      sii = sort (nub (getReachableStatesFromSi si transitions))
  in compareReachableStateGroups si sii transitions

createReachableEndStates :: [String] -> [String] -> [String]
createReachableEndStates reachables ends = sort (intersect reachables ends)

-- ****************** ELIMINACIA NEDOSIAHNUTELNYCH PRECHODOV **********

createReachableTransitions :: [Transition] -> [String] -> [Transition]
createReachableTransitions _ [] = []
createReachableTransitions [] _ = []
createReachableTransitions (x:xs) states =
  if ((from x) `elem` states) && ((to x) `elem` states)
    then [x] ++ createReachableTransitions xs states
    else createReachableTransitions xs states

-- ****************** PRECHODY DO SINK *****************************

createSinkTransition :: String -> String -> Transition
createSinkTransition state symbolFromAlphabet = 
  Transition {
    from = state,
    symbol = symbolFromAlphabet,
    to = sink
  }

getSinkTransitionsForStateAndSymbol :: String -> [Transition] -> String -> [Transition]
getSinkTransitionsForStateAndSymbol state [] symbolFromAlphabet = [createSinkTransition state symbolFromAlphabet]
getSinkTransitionsForStateAndSymbol state (x:xs) symbolFromAlphabet =
  if (state == (from x)) && (symbolFromAlphabet == (symbol x))
    then []
    else getSinkTransitionsForStateAndSymbol state xs symbolFromAlphabet

getSinkTransitionsForState :: String -> [Transition] -> [String] -> [Transition]
getSinkTransitionsForState state transitions [] = []
getSinkTransitionsForState state transitions (x:xs) = getSinkTransitionsForStateAndSymbol state transitions x ++ getSinkTransitionsForState state transitions xs

addSinkTransitions :: [String] -> [Transition] -> [String] -> [Transition]
addSinkTransitions [] transitions alphabet = []
addSinkTransitions (x:xs) transitions alphabet = getSinkTransitionsForState x transitions alphabet ++ addSinkTransitions xs transitions alphabet

addSinkState :: Int -> [String]
addSinkState sinkTransitions = 
  if sinkTransitions > 0
    then ["sink"]
    else []

addSinkToSinkTransitions :: [String] -> [Transition]
addSinkToSinkTransitions [] = []
addSinkToSinkTransitions (x:xs) = [createSinkTransition sink x] ++ (addSinkToSinkTransitions xs)


-- ****************** UPLNY AUTOMAT *****************************

makeFullyDefinedDKA :: DKA -> DKA
makeFullyDefinedDKA dka = 
  let reachableStates = createReachableStates (start dka) (transitions dka)
      reachableTransitions = createReachableTransitions (transitions dka) reachableStates
      sinkTransitions = addSinkTransitions reachableStates reachableTransitions (alphabet dka)
      sinkToSinkTransitions = addSinkToSinkTransitions (alphabet dka)
  in DKA {
      states = reachableStates ++ (addSinkState (length sinkTransitions)),
      start = start dka,
      end = createReachableEndStates reachableStates (end dka),
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
    ++ (show (endGroup eq)) ++ ")")

printEqGroup :: EqGroup -> IO ()
printEqGroup eq = do
  putStrLn ("group " ++ (show (number eq)))
  putStrLn (intercalate "," (stateList eq))
  mapM_ printEqTrans (transList eq)

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


printEqClass :: Test -> IO ()
printEqClass eq = do
  putStrLn ("GSL: ")
  mapM_ printGSL (groupStateList eq)
  putStrLn ("GROUPS: ")
  mapM_ printEqGroup (groups (eqClass eq))
  putStrLn ("pocet skupin: " ++ show (groupCount eq))
  mapM_ printTestTrans (trans eq)
  putStrLn ("new")
  mapM_ printTestTrans (trans2 eq)
  putStrLn ("zipped")
  mapM_ printZipped (zipped eq)

createEqTransition :: Transition -> DKA -> EqTransition
createEqTransition trans dka = 
  if (to trans) `elem` (end dka)
    then 
      EqTransition {
        transition = trans,
        endGroup = 1
      }
    else
      EqTransition {
        transition = trans,
        endGroup = 2
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
    if (state) `elem` (end dka)
      then (createEqTransitions state dka) ++ eqStates
      else eqStates) 
  [] (states dka)

getNotEndEqTransitions :: DKA -> [EqTransition]
getNotEndEqTransitions dka =
  foldr (\state eqStates -> 
    if (state) `elem` (end dka)
      then eqStates
      else (createEqTransitions state dka) ++ eqStates) 
  [] (states dka)

getStateListFromTransitions :: [EqTransition] -> [String]
getStateListFromTransitions [] = []
getStateListFromTransitions (x:xs) = [from (transition x)] ++ getStateListFromTransitions xs

createEqGroup :: Int -> [EqTransition] -> EqGroup
createEqGroup num transitions = 
  let stateList = sort (nub (getStateListFromTransitions transitions))
  in
  EqGroup {
    number = num,
    stateList = stateList,
    transList = transitions
  }

createEqGroups0 :: DKA -> [EqGroup]
createEqGroups0 dka = 
  let endGroup = createEqGroup 1 (getEndEqTransitions dka)
      notEndGroup = createEqGroup 2 (getNotEndEqTransitions dka)
  in [endGroup, notEndGroup]

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
getEndGroups (x:xs) = [endGroup x] ++ getEndGroups xs

checkEndGroups :: [EqGroup] -> [(Int, [Int])]
checkEndGroups [] = []
checkEndGroups (x:xs) = [(number x, sort (nub (getEndGroups (transList x))))] ++ checkEndGroups xs

-- getCorrectGroup :: String -> [EqGroup] -> EqGroup
-- getCorrectGroup num (x:xs) =
--   if num == (number x)
--     then x
--     else getCorrectGroup num xs

-- getGroupTransition :: String -> [EqTransition] -> [EqTransition]
-- getGroupTransition _ [] = []
-- getGroupTransition num (x:xs) = 
--   if num == (endGroup x)
--     then [x] ++ getGroupTransition num xs
--     else getGroupTransition num xs

-- getGroupTransitions :: String -> [String] -> [EqTransition] -> [[EqTransition]]
-- getGroupTransitions _ _ [] = []
-- getGroupTransitions _ [] _ = []
-- getGroupTransitions f (x:xs) trans = [getGroupTransition x trans] ++ getGroupTransitions f xs trans 

-- splitTransitions :: [(String, [String])] -> [EqGroup]-> [(String, [[EqTransition]])]
-- splitTransitions [] _ = []
-- splitTransitions _ [] = []
-- splitTransitions (x:xs) groups = 
--   let correctGroup = getCorrectGroup (fst x) groups
--       groupTransitions = getGroupTransitions (fst x) (snd x) (transList correctGroup)
--   in [(fst x, groupTransitions)] ++ (splitTransitions xs groups)

-- transitionsWithSymbol :: String -> [[EqTransition]] -> [EqTransition]
-- transitionsWithSymbol _ [] = []
-- transitionsWithSymbol symbol (x:xs) =
--   if symbol == (symbol (transition x))
--     then [x] ++ transitionsWithSymbol symbol xs
--     else transitionsWithSymbol symbol xs

-- splitTransitionsForGroup :: [[EqTransition]] -> [(String, [String])] -> [String] -> [[EqTransition]]
-- splitTransitionsForGroup [] _ _ = []
-- splitTransitionsForGroup _ [] _ = []
-- splitTransitionsForGroup _ _ [] = []
-- splitTransitionsForGroup transList groupStateList (x:xs) = 
--   let trans = transitionsWithSymbol x transList
--       endGroups = sort (nub (getEndGroups trans))
--   in
--     if (length endGroups) == 1
--       then splitTransitionsForGroup transList groupStateList xs
--       else 


-- splitTransitions :: [EqGroup] -> [(String, [String])] -> [String] -> [(String, [[EqTransition]])]
-- splitTransitions [] _ _ = []
-- splitTransitions _ [] _ = []
-- splitTransitions _ _ [] = []
-- splitTransitions (x:xs) groupStateList alphabet = splitTransitionsForGroup [(transList x)] groupStateList alphabet ++ splitTransitions xs groupStateList alphabet


getCurrentTransitions :: [EqGroup] -> [(Int, [[EqTransition]])]
getCurrentTransitions [] = []
getCurrentTransitions (x:xs) = [((number x), [(transList x)])] ++ getCurrentTransitions xs

createGroupStateList :: [EqGroup] -> [(Int, [String])]
createGroupStateList [] = []
createGroupStateList (x:xs) = [((number x), (stateList x))] ++ createGroupStateList xs

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
  if ((symbol (transition x)) == sym) && ((endGroup x) == groupNumber)
    then [from (transition x)] ++ findAll groupNumber xs sym
    else findAll groupNumber xs sym

getTrans :: [String] -> [EqTransition] -> [EqTransition]
getTrans [] _ = []
getTrans _ [] = []
getTrans states (x:xs) =
  if (from (transition x) `elem` states)
    then [x] ++ getTrans states xs
    else getTrans states xs


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
newTransitions (x:xs) alphabet endGroupCheck = 
  let groupCheckList = getGroupCheckList endGroupCheck (fst x) -- ["I","II"]
      a = filter (not . null) (newTransitions2 (snd x) alphabet groupCheckList)
  in [((fst x), a)] ++ newTransitions xs alphabet endGroupCheck
  -- in a ++ newTransitions xs alphabet endGroupCheck

  -- if (length (snd x)) > alphabetLength
    -- then newTransitions xs alphabetLength
    -- else [x] ++ newTransitions xs alphabetLength

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
        endGroup = newEndGroup
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


reduceEqClass :: EqClass -> [String] -> Test
reduceEqClass eqClassI alphabet = 
  let endGroupCheck = checkEndGroups (groups eqClassI) 
      groupCount = length endGroupCheck
      i = getCurrentTransitions (groups eqClassI)
      -- ii = splitTransitions (groups eqClassI) groupStateList alphabet
      ii = newTransitions i alphabet endGroupCheck
      renamedGroups = renameGroups ii 1
      newGSL = createNewGSL renamedGroups
      fixedEndGroups = fixEndGroups renamedGroups newGSL
  in 
    Test {
      eqClass = eqClassI,
      trans = i,
      trans2 = fixedEndGroups,
      groupCount = groupCount,
      -- groupStateList = createGroupStateList (groups eqClassI),
      groupStateList = newGSL,
      zipped = []
    }

-- TODO fix
makeReducedDKA :: DKA -> Test
makeReducedDKA dka = 
  let eqClass0 = createEqClass0 dka
      eqClassMax = reduceEqClass eqClass0 (alphabet dka)
  in  eqClassMax
  -- in DKA {
  --   states = states dka,
  --   start = start dka,
  --   end = end dka,
  --   transitions = transitions dka,
  --   alphabet = alphabet dka
  -- }

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
        --TODO potom zmenit        
          "-t" -> printEqClass (makeReducedDKA (makeFullyDefinedDKA formattedInput))
          _ -> error "Chybny prepinac"
        hClose handle

    return ()