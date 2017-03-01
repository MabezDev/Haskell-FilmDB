-- 
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

--
-- Types
--
-- Define Film type here 

--testDatabase :: [Film]
--testDatabase = [ ... the 25 Film values ... ]

data Film = Film String String Int [String] deriving (Read, Show)

-- 
--
--  Your functional code goes here
--
--

filmFromString :: String -> Film
filmFromString line = 
    read line :: Film
    
--databaseFromStringArray :: [String] -> [Film]
--databaseFromStringArray []      =  []
--databaseFromStringArray (x:xs)  = filmFromString x ++ databaseFromStringArray xs

databaseFromStringArray :: [String] -> [Film]
databaseFromStringArray linesArray = [ filmFromString line | line <- linesArray]    

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
    
    
