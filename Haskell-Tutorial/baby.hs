-- vynasobi parameter dvojkou
doubleMe x = x + x 

-- vynasobi oba parametre dvojkou a potom vysledky scita
doubleUs x y = doubleMe x + doubleMe y   

-- ak x > 100 tak ho vrati, inak ho vynasobi dvomi
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2   

-- vlastna dlzka listu, kazdy prvok nahradi jednickou a potom ich scita                        
length' xs = sum [1 | _ <- xs]   

-- prvy riadok je deklaracia typu, berie jeden parameter typu string, a vracia string
-- zo stringu necha len velke pismena, ostatne zmaze
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

-- pattern matching, nieco ako velky if else, s presnymi hodnotami pre urcite vstupy
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

-- dlzka listu, spravena cez rekurziu a pattern matching
length1' :: (Num b) => [a] -> b  
length1' [] = 0  
length1' (_:xs) = 1 + length' xs  

-- priklad na guards, krajsi zapis velkeho if else
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  


-- vypocet bmi, z dvoch vstupnych hodnot, pouzitie where namiesto 3x sa opakovania vypoctu na kazdom riadku
bmiTell1 :: (RealFloat a) => a -> a -> String  
bmiTell1 weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  

-- let binding, je to nieco ako where
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- rekurzivne definovane maximum z nejakeho listu, max je existujuca funkcia ktora porovnava 2 parametre
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)     

-- vezme n hodnot z listu 
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

-- otoci list
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

-- quicksort pomocou rekurzie
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- quicksort ale s filter funkciou
quicksort1 :: (Ord a) => [a] -> [a]    
quicksort1 [] = []    
quicksort1 (x:xs) =     
    let smallerSorted = quicksort1 (filter (<=x) xs)  
        biggerSorted = quicksort1 (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted      

-- najvacsie cislo mensie ako 100000 ktore je delitelne cislom 3829
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0  

-- suma spravena cez left fold, fold berie ako prvy parameter funkciu, ktoru aplikuje na druhy a treti parameter
-- treti parameter je list takze fold sa opakuje pre kazdy prvok listu
-- ako navratova hodnota je hodnota akumulatoru (2. parametru) po prebehnuti vsetkych opakovani
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 

