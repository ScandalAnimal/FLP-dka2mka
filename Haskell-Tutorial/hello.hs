main = do
    putStrLn "Hello world, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ".")