import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = notAvailable command

notAvailable :: String -> [String] -> IO ()
notAvailable command _ = putStrLn $ "Sorry, The " ++ command ++ " is not available yet."

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "Number of arguments doesnot match. Expected 2 (filename, todoItem)."

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith(\num line -> show num ++ ". " ++ line) [1..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "Number of arguments doesnot match. Expected 1 (filename)."

remove :: [String] -> IO ()
remove [fileName, numString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numString
        newTodoItems = unlines $ delete (todoTasks !! (number - 1)) todoTasks
    bracketOnError (openTempFile "." "temp") 
        (\(tempFileName, tempHandle) -> do
            hClose tempHandle
            removeFile tempFileName)
        (\(tempFileName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempFileName fileName)
remove _ = putStrLn "Number of arguments doesnot match. Expected 2 (filename, todoItemNumber)."

bump :: [String] -> IO ()
bump [fileName, numString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numString
        itemToBump = (todoTasks !! (number - 1))
        newTodoItems = unlines $ [itemToBump] ++ delete (todoTasks !! (number -1)) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempFileName, tempHandle) -> do
            hClose tempHandle
            removeFile tempFileName)
        (\(tempFileName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempFileName fileName)
bump _ = putStrLn "Number of arguments doesnot match. Expected 2 (filename, todoItemNumber)."
