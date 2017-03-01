-- 
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- UP745497
--

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

addFilmToDatabase :: [Film] -> Film -> [Film] --I
addFilmToDatabase database newFilm = (newFilm:database)

filmFromString :: String -> Film
filmFromString line = 
    read line :: Film -- cast to Film just incase
    
filmToStringFormatted :: Film -> String
filmToStringFormatted (Film title director years fansArray) = 
    "[Film]\n" ++ "\tTitle: " ++ title ++ "\n\tDirector: " ++ director 
    ++ "\n\tRelease year: " ++ (show years) ++ "\n\tFans:" 
    ++ (show fansArray) ++ "\n" 

databaseToStringFormatted :: [Film] -> [String] --II
databaseToStringFormatted filmsArray = [filmToStringFormatted film | film <- filmsArray]    

databaseFromStringArray :: [String] -> [Film]
databaseFromStringArray linesArray = [ filmFromString line | line <- linesArray]

searchFilmByYearReleased :: [Film] -> Int -> [Film] --III
searchFilmByYearReleased filmdb yearAfter = 
    [(Film title director year fans) | (Film title director year fans) <- filmdb, yearAfter > year]
    
searchFilmByFan :: [Film] -> String -> [Film]  --IV
searchFilmByFan filmdb fan = 
    [(Film title director year fans) | (Film title director year fans) <- filmdb, containsString fans fan]

searchFansByFilm :: [Film] -> String -> [String] --V
searchFansByFilm filmdb title = getFans (findFilmByTitle filmdb title)

addFanToFilm :: [Film] -> String -> String -> [Film] --VI
addFanToFilm [] _ _ = []
addFanToFilm ((Film title director year fans):xs) titleToModify fanToAdd = 
    if title == titleToModify
        then (Film title director year (fanToAdd : fans)) : addFanToFilm xs titleToModify fanToAdd
        else (Film title director year fans) : addFanToFilm xs titleToModify fanToAdd
        
searchFansByDirector :: [Film] -> String -> [String] -> [String] --VII
searchFansByDirector [] _ _  = []
searchFansByDirector ((Film title director year fans):xs) directorToFind listOfFans =
    if director == directorToFind
        then searchFansByDirector xs directorToFind (listOfFans++[ fan | fan <- fans, not (containsString listOfFans fan)])
        else searchFansByDirector xs directorToFind listOfFans
        
        

        


 

--
--
-- Helper functions
--
--

findFilmByTitle :: [Film] -> String -> Film
findFilmByTitle [] _ = (Film "" "" 0 [])
findFilmByTitle ((Film title director year fans):xs) titleToFind = 
    if title == titleToFind 
       then (Film title director year fans) 
       else  findFilmByTitle xs titleToFind

containsString :: [String] -> String -> Bool
containsString [] _ = False
containsString (x:xs) target = if x == target then True else containsString xs target


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

readInDB :: String -> IO [Film]
readInDB filename = do
    content <- readFile filename
    let lineArray = lines content
    return (databaseFromStringArray lineArray)
    
    
printMenu :: IO ()
printMenu = do
        putStrLn ("1) Query this")
        putStrLn ("2) Add that")
    
    
