-- FLP projekt
-- varianta: dka-2-mka
-- autor: Maros Vasilisin, xvasil02

import System.Environment
import System.IO
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char

-- vlastna reprezentacia prechodov
data Transition = Transition {
  from::String,
  char::String,
  to::String
} deriving (Show)

-- vlastna reprezentacia DKA
data DKA = DKA {
  states::[String],
  start::[String],
  end::[String],
  alphabet::[String],
  transitions::[Transition]
} deriving (Show)

-- prepinac -i
printRichDKA :: DKA -> IO ()
printRichDKA dka = do
  putStrLn "States:"
  putStrLn (intercalate "," (states dka))
  putStrLn "Start state:"
  putStrLn (intercalate "," (start dka))
  putStrLn "End states:"
  putStrLn (intercalate "," (end dka))
  putStrLn "Alphabet:"
  putStrLn (intercalate "," (alphabet dka))
  putStrLn "Transitions:"
  mapM_ printTrans (transitions dka)

-- prepinac -t
printDKA :: DKA -> IO ()
printDKA dka = do
  putStrLn (intercalate "," (states dka))
  putStrLn (intercalate "," (start dka))
  putStrLn (intercalate "," (end dka))
  mapM_ printTrans (transitions dka)  

printTrans :: Transition -> IO ()
printTrans trans = do
  putStr (from trans)
  putStr ","
  putStr (char trans)
  putStr ","
  putStrLn (to trans) 

-- podla poctu argumentov vrati stdin alebo otvoreny subor
getHandle :: [FilePath] -> IO Handle
getHandle args = if (length args == 1)
                  then return stdin
                  else openFile (args!!1) ReadMode

-- nacitanie vstupu zo stdin (http://learnyouahaskell.com/input-and-output)
readFromStdin :: Handle -> String -> IO String
readFromStdin handle lines = do
  line <- hGetLine handle
  if null line
    then return (lines ++ line)
    else readFromStdin handle (lines ++ line ++ "\n")

-- podla poctu argumentov nacitanie zo stdin alebo zo suboru
getContentsFromInput :: Handle -> [a] -> IO String
getContentsFromInput handle args = if (length args == 1)
                            then readFromStdin handle ""
                            else hGetContents handle

--https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
customSplit :: Eq a => a -> [a] -> [[a]]
customSplit d [] = []
customSplit d s = x : customSplit d (drop 1 y) where (x,y) = span (/= d) s

parseAlphabet :: [String] -> [String]
parseAlphabet [x] = [((customSplit ',' x)!!1)] 
parseAlphabet (x:xs) = [((customSplit ',' x)!!1)] ++ parseAlphabet xs 

parseTransition :: [String] -> Transition
parseTransition x = Transition {
  from = x!!0,
  char = x!!1,
  to = x!!2
}

makeTransitions :: [String] -> [Transition]
makeTransitions [] = []
makeTransitions [x] = [parseTransition (customSplit ',' x)]
makeTransitions (x:xs) = [(parseTransition (customSplit ',' x))] ++ makeTransitions xs

formatInput :: [String] -> DKA
formatInput lines = DKA {
  states = (customSplit ',' (lines!!0)),
  start = (customSplit ',' (lines!!1)),
  end = (customSplit ',' (lines!!2)),
  alphabet = (sort (nub (parseAlphabet (drop 3 lines)))),
  transitions = makeTransitions (drop 3 lines)
}

-- fake vypis TODO zmenit podla alg z TINu
minimize :: DKA -> DKA
minimize dka = DKA {
  states = states dka,
  start = start dka,
  end = end dka,
  transitions = transitions dka,
  alphabet = alphabet dka
}

-- vstupny bod programu
main = do
    args <- getArgs
    let argCount = length args
    if (argCount == 0 || argCount > 2)
      then error "Chybny pocet argumentov" -- osetrenie argumentov programu
      else do
        handle <- getHandle args
        contents <- getContentsFromInput handle args

        let formattedInput = formatInput (lines contents)
        
        case (args!!0) of 
          "-i" -> printRichDKA formattedInput
          "-t" -> printDKA (minimize formattedInput)
          _ -> error "Chybny prepinac"
        hClose handle

    return ()