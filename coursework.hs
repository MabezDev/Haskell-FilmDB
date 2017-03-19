-- 
-- MATHFUN
-- UP745497
--

--
-- Imports
--

import Data.List

--
-- Types
--
-- Define Film type here 

data Film = Film String String Int [String] deriving (Read, Show)

-- Test Data --
testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe","Heidi","Jo","Kate","Emma","Liz","Sam","Olga","Tim"],
                Film "The Fly" "David Cronenberg" 1986 ["Garry","Dave","Zoe","Kevin","Emma"],
                Film "Body Of Lies" "Ridley Scott" 2008 ["Bill","Olga","Tim","Zoe","Paula"],
                Film "Avatar" "James Cameron" 2009 ["Dave","Amy","Liz"],
                Film "Titanic" "James Cameron" 1997 ["Zoe","Emma","Paula","Liz","Olga","Dave"],
                Film "The Departed" "Martin Scorsese" 2006 ["Wally","Liz","Kevin","Tim","Emma"],
                Film "Aliens" "Ridley Scott" 1986 ["Dave","Garry","Liz","Sam","Wally","Kate","Zoe"],
                Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo","Wally","Emma"],
                Film "Prometheus" "Ridley Scott" 2012 ["Kevin","Tim","Emma","Jo","Liz"],
                Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave","Amy","Garry","Ian","Neal"],
                Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally","Sam","Dave","Neal"],
                Film "Jaws" "Steven Spielberg" 1975 ["Dave","Jo","Zoe","Wally","Emma","Kate"],
                Film "The Martian" "Ridley Scott" 2015 ["Wally","Sam","Dave","Jo","Jenny","Kate","Emma","Olga"],
                Film "The BFG" "Steven Spielberg" 2016 ["Sam","Wally","Dave","Jo","Kate"],
                Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave","Amy","Bill","Garry","Ian","Neal","Kate","Jenny","Zoe"],
                Film "Gladiator" "Ridley Scott" 2000 ["Olga","Neal","Kate","Heidi","Bill","Sam","Zoe"],
                Film "The Green Mile" "Frank Darabont" 1999 ["Kevin","Tim","Emma","Heidi"],
                Film "True Lies" "James Cameron" 1994 ["Sam","Dave"],
                Film "Super 8" "J J Abrams" 2011 ["Kevin","Tim","Emma","Olga","Heidi"],
                Film "Minority Report" "Steven Spielberg" 2002 ["Kevin","Kate","Tim","Emma","Olga","Jenny","Zoe"],
                Film "War Horse" "Steven Spielberg" 2011 ["Garry","Bill","Olga","Jo","Wally","Emma","Tim","Kate","Zoe"],
                Film "Silence" "Martin Scorsese" 2016 ["Wally","Emma","Tim","Heidi","Bill","Olga","Jo"],
                Film "The Terminal" "Steven Spielberg" 2004 ["Kate","Dave","Jo","Wally","Emma"],
                Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma","Wally","Zoe","Kate","Bill","Dave","Liz","Jo"],
                Film "Hugo" "Martin Scorsese" 2011 ["Wally","Sam"]]

-- 
--
--  Your functional code goes here
--
--

addFilmToDatabase :: [Film] -> String -> String -> Int -> [String] -> [Film] --I
addFilmToDatabase database  title director year fans = ((Film title director year fans):database)
    
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

frequency :: [String] -> [(String, Int)]
frequency listOfDirectors = nub [  (director, instances listOfDirectors director) | director <- listOfDirectors]

instances:: [String] -> String -> Int
instances [] _ = 0
instances (y:ys) director 
    | director == y = 1+(instances ys director)
    | otherwise = instances ys director


getTitle :: Film -> String
getTitle (Film t _ _ _) = t

getDirector :: Film -> String
getDirector (Film _ d _ _) = d

getYear :: Film -> Int
getYear (Film _ _ y _) = y

getFans :: Film -> [String]
getFans (Film _ _ _ f) = f

-- film exists
filmExists :: [Film] -> String -> Bool -- FilmDB and film title
filmExists [] _ = False
filmExists (x:xs) title
    | getTitle x == title = True || filmExists [] title
    | otherwise           = False || filmExists xs title

fanExists :: [Film] -> String -> String -> Bool -- FilmDB, film title and Fan name
fanExists [] _ _ = False
fanExists (x:xs) title fan
    | getTitle x == title = (fan `elem` getFans x) || fanExists [] title fan
    | otherwise           = False || fanExists xs title fan



-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1  = printFilmArray $ addFilmToDatabase testDatabase "Alien: Covenant" "Ridley Scott" 2017 []
demo 2  = printFilmArray testDatabase
demo 3  = printFilmArray (searchFilmByYearReleased testDatabase 2008)
demo 4  = printFilmArray (searchFilmByFan testDatabase "Liz")
demo 5  = putStrLn (show (searchFansByFilm testDatabase "Jaws"))
demo 6  = printFilmArray (addFanToFilm testDatabase "The Fly" "Liz")-- putStrLn all films after "Liz" says she becomes fan of "The Fly"
demo 66  = printFilmArray (addFanToFilm testDatabase "Avatar" "Liz")--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
demo 7 =  putStrLn (show (searchFansByDirector testDatabase "James Cameron")) --putStrLn all fans of films directed by "James Cameron"
demo 8  = putStrLn (show (searchDirectorsByFan testDatabase "Liz")) --putStrLn all directors & no. of their films that "Liz" is a fan of

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

isInt :: String -> Bool
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
            if filmExists filmdb title
                then do
                    putStrLn "A film already exists with this title!"
                    handleInput filmdb username
                else 
                    return () -- not the normal return, the program just carries on
            putStrLn "Director: "
            director <- getLine
            putStrLn "Year: "
            year <- getInt
            if year /= -1 && year > 0
                then handleInput (addFilmToDatabase filmdb title director year []) username
                else do
                    putStrLn "Not a valid year!" 
                    handleInput filmdb username
        2 -> do
            printFilmArray filmdb
            handleInput filmdb username
        3 -> do
            putStrLn "Films after the year: "
            year <- getInt
            if year /= -1 && year > 0
                then do
                    printFilmArray (searchFilmByYearReleased filmdb year)
                    handleInput filmdb username
                else do
                    putStrLn "Not a valid year!" 
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
            if fanExists filmdb title username
                then do
                    putStrLn ("You are already a fan of "++title)
                    handleInput filmdb username
                else handleInput (addFanToFilm filmdb title username) username
            
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
            --saveDatabase filmdb "films.txt"  -- save to file here
            --putStrLn ("Exiting...")
            return ()
        -1 -> do
            putStr "Enter a valid Integer!"
            putStrLn ""
            handleInput filmdb username
        _ -> do
            putStrLn "Not a valid option enter an integer between 1 and 9."
            handleInput filmdb username
        
    
saveDatabase :: [Film] -> String -> IO ()
saveDatabase filmdb filename = writeFile filename (show filmdb)
    
    
        
main :: IO ()
main = do
    dbRaw <- readFile "films.txt"
    let db = (read dbRaw :: [Film])
    putStrLn ""
    printFilmArray db
    putStrLn ""
    username <- askName
    putStrLn ""
    putStrLn ("Welcome " ++ username)
    handleInput db username
    
    
