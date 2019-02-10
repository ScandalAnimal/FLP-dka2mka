-- FLP projekt
-- varianta: dka-2-mka
-- autor: Maros Vasilisin, xvasil02

import System.Environment
import System.IO
import Debug.Trace

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

-- vstupny bod programu
main = do
    args <- getArgs
    let argCount = length args
    if (argCount == 0 || argCount > 2)
      then error "Chybny pocet argumentov" -- osetrenie argumentov programu
      else do
        handle <- getHandle args
        contents <- getContentsFromInput handle args

        hClose handle



    return ()