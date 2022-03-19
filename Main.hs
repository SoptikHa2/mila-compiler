import Parse.Parser (getAST)
import System.Environment

main :: IO ()
main = getArgs >>= parseArgs >>= putStrLn

parseArgs :: [String] -> IO String
parseArgs [] = return "No input filename given."
parseArgs [filename] = do
    file <- readFile filename
    return (show (getAST filename file))
parseArgs (x:xs) = parseArgs [x]
