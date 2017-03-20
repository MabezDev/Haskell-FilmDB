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

data Query = Search String Int | Remove String Int 
-- Search for string in field 0 a.k.a Search for the String in title as title is the first element in the tuple

-- 
--
--  Your functional code goes here
--
--

addFilmToDatabase :: [Film] -> Film -> [Film]
addFilmToDatabase database newFilm = (newFilm:database)

filmFromString :: String -> Film
filmFromString line = 
    read line :: Film -- cast to Film just incase
    
filmToStringFormatted :: Film -> String
filmToStringFormatted (Film title director years fansArray) = 
    "[Film]\n" ++ "\tTitle: " ++ title ++ "\n\tDirector: " ++ director 
    ++ "\n\tRelease year: " ++ (show years) ++ "\n\tFans:" 
    ++ (show fansArray) ++ "\n" 

databaseToStringFormatted :: [Film] -> [String]
databaseToStringFormatted filmsArray = [filmToStringFormatted film | film <- filmsArray]    

databaseFromStringArray :: [String] -> [Film]
databaseFromStringArray linesArray = [ filmFromString line | line <- linesArray]


handleQuery :: Query -> [Film]
handleQuery  (Search keyword field) = searchDB keyword field

searchDB :: String -> Int -> [Film]
searchDB keyword field = []

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
    
    
