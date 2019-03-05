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
} deriving (Show)

-- vlastna reprezentacia DKA
data DKA = DKA {
  states :: [String],
  alphabet :: [String],
  transitions :: [Transition],
  start :: [String],
  end :: [String]
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


-- ****************** WIP *****************************

-- fake vypis TODO zmenit podla alg z TINu
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
          "-t" -> printCustomDKA (makeFullyDefinedDKA formattedInput)
          _ -> error "Chybny prepinac"
        hClose handle

    return ()