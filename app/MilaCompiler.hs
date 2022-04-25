import Parse.Parser (getAST)
import System.Environment
import System.Exit
import Data.Either

main :: IO ()
main = getArgs >>= parseArgs >>= putStrLnAndExit

parseArgs :: [String] -> IO (String, ExitCode)
parseArgs [] = return ("No input filename given.", ExitFailure 64)
parseArgs [filename] = do
    file <- readFile filename
    let ast = getAST filename file
    if isLeft ast then return (show ast, ExitFailure 1) else return (show ast, ExitSuccess)
parseArgs (x:xs) = parseArgs [x]

putStrLnAndExit :: (String, ExitCode) -> IO a
putStrLnAndExit (str, ec) = putStrLn str >> exitWith ec
