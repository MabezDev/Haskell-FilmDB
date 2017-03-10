-- 
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- UP745497
--

--
-- Imports
--

import Control.Monad
import Data.List
import Data.Map (fromListWith, toList)
import Data.Char

--
-- Types
--
-- Define Film type here 

data Film = Film String String Int [String] deriving (Read, Show)

-- 
--
--  Your functional code goes here
--
--

addFilmToDatabase :: [Film] -> String -> String -> Int -> [String] -> [Film] --I
addFilmToDatabase database  title director year fans = ((Film title director year fans):database)

filmFromString :: String -> Film
filmFromString line = 
    read line :: Film -- cast to Film just incase
    
filmToStringFormatted :: Film -> String
filmToStringFormatted (Film title director years fansArray) = 
    "[Film]\n" ++ "\tTitle: " ++ title ++ "\n\tDirector: " ++ director 
    ++ "\n\tRelease year: " ++ (show years) ++ "\n\tNumber of fans: " 
    ++ (show (length fansArray)) ++ "\n" 

databaseToStringFormatted :: [Film] -> [String] --II
databaseToStringFormatted filmsArray = [filmToStringFormatted film | film <- filmsArray]    

searchFilmByYearReleased :: [Film] -> Int -> [Film] --III
searchFilmByYearReleased filmdb yearAfter = 
    [(Film title director year fans) | (Film title director year fans) <- filmdb, yearAfter < year]
    
searchFilmByFan :: [Film] -> String -> [Film]  --IV
searchFilmByFan filmdb fan = 
    [(Film title director year fans) | (Film title director year fans) <- filmdb, fan `elem` fans]

searchFansByFilm :: [Film] -> String -> [String] --V
searchFansByFilm filmdb title = concat [ (getFans film) | film <- filmdb, title == (getTitle film)]

--addFanToFilm :: [Film] -> String -> String -> [Film] --VI
--addFanToFilm [] _ _ = []
--addFanToFilm ((Film title director year fans):xs) titleToModify fanToAdd = 
--    if title == titleToModify
--        then (Film title director year (fanToAdd : fans)) : addFanToFilm xs titleToModify fanToAdd
--        else (Film title director year fans) : addFanToFilm xs titleToModify fanToAdd

--addFanToFilm :: [Film] -> String -> String -> [Film] --TODO fix bug with this where if a fan is only following one film it throws a Non Exaustive pattern excetion
--addFanToFilm (x:xs) title fan
--    | (length (x:xs)) < 1  = []
--    | (getTitle x) == title = (Film title (getDirector x) (getYear x) (fan:(getFans x))) : (addFanToFilm xs title fan)
--    | otherwise             = x : (addFanToFilm xs title fan)

addFanToFilm :: [Film] -> String -> String -> [Film]
addFanToFilm filmdb title fan
    | null filmdb  = []
    | (getTitle film) == title = (Film title (getDirector film) (getYear film) (fan:(getFans film))) : (addFanToFilm rest title fan)
    | otherwise                = film : (addFanToFilm rest title fan)
    where
        film = head filmdb
        rest = tail filmdb
        
        

searchFansByDirector :: [Film] -> String -> [String] --VII
searchFansByDirector filmdb directorToFind = nub (concat [ (getFans film) | film <- filmdb, directorToFind == (getDirector film)])


searchDirectorsByFan :: [Film] -> String -> [(String,Int)] --VIII   
searchDirectorsByFan filmdb fan = frequency [ (getDirector film) | film <- (searchFilmByFan filmdb fan)]

--
--
-- Helper functions
--
--

-- fromList combines a list tuples to create a map, toList creates List from a map
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs]) -- mapping first tuple to count

getTitle :: Film -> String
getTitle (Film t d y f) = t

getDirector :: Film -> String
getDirector (Film t d y f) = d

getYear :: Film -> Int
getYear (Film t d y f) = y

getFans :: Film -> [String]
getFans (Film t d y f) = f


-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2017 film "Alien: Covenant"
--                   by "Ridley Scott" to testDatabase
--demo 2  = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films released after 2008
--demo 4  = putStrLn all films that "Liz" is a fan of
--demo 5  = putStrLn all fans of "Jaws"
--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
--demo 7 =  putStrLn all fans of films directed by "James Cameron"
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of

--
--
-- Your user interface code goes here
--
--

printFilmArray :: [Film] -> IO ()
printFilmArray [] = return ()
printFilmArray (x:xs) = do
    putStrLn (filmToStringFormatted x)
    printFilmArray xs
    
    
printMenu :: IO ()
printMenu = do
    putStrLn ""
    putStrLn ("1) Add a new film.")
    putStrLn ("2) List all films.")
    putStrLn ("3) List all films after a certain year.")
    putStrLn ("4) List all films you are a fan of.")
    putStrLn ("5) List all fans of a particular film.")
    putStrLn ("6) Become a fan of a film.")
    putStrLn ("7) List all fans of a director.")
    putStrLn ("8) List directors and the number of films you are a fan of.")
    putStrLn ("9) Exit.")
    putStr "--> "
       
askName :: IO String
askName = do
        putStrLn "What is your name?"
        getLine
        
getInt :: IO Int
getInt = do
    input <- getLine
    if isInt input
        then return (read input :: Int)
        else return (-1)

isInt s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False       
        
handleInput :: [Film] -> String -> IO ()
handleInput filmdb username = do
    printMenu
    choice <- getInt
    putStrLn ""
    
    case choice of
        1 -> do
            putStrLn "Title: "
            title <- getLine
            putStrLn "Director: "
            director <- getLine
            putStrLn "Year: "
            year <- getInt
            handleInput (addFilmToDatabase filmdb title director year []) username
        2 -> do
            printFilmArray filmdb
            handleInput filmdb username
        3 -> do
            putStrLn "Films after the year: "
            year <- getInt
            printFilmArray (searchFilmByYearReleased filmdb year)
            handleInput filmdb username
        4 -> do
            putStrLn ("Listing all films that " ++ username ++ " is a fan of.")
            printFilmArray (searchFilmByFan filmdb username)
            handleInput filmdb username
        5 -> do
            putStrLn "Film: "
            title <- getLine
            putStrLn (show (searchFansByFilm filmdb title))
            handleInput filmdb username
        6 -> do
            putStrLn "Film: "
            title <- getLine
            handleInput (addFanToFilm filmdb title username) username
        7 -> do
            putStrLn "Director: "
            director <- getLine
            putStrLn (show (searchFansByDirector filmdb director))
            handleInput filmdb username
        8 -> do
            putStrLn ("Listing all directors and the number of films " ++ username ++ " is a fan of. In the format ('Director',number).")
            putStrLn (show (searchDirectorsByFan filmdb username))
            handleInput filmdb username
        9 -> do
            putStrLn "Saving database to 'films.txt'"
            -- save to file here
            -- saveDatabase filmdb "films.txt"
            putStrLn ("Exiting...")
            return ()
        -1 -> do
            putStr "Enter a valid Integer!"
            putStrLn ""
            handleInput filmdb username
        _ -> do
            putStrLn "Not a valid option enter an integer between 1 and 9."
            handleInput filmdb username
        
    
saveDatabase :: [Film] -> String -> IO ()
saveDatabase filmdb filename = do
        writeFile filename (show filmdb)
    
    
        
main :: IO ()
main = do
    username <- askName
    db <- readFile "films.txt"
    handleInput (read db :: [Film]) username
    
    
