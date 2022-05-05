import Parse.Parser (getAST)
import StaticAnalysis.Rewrite (replaceFunc)
import IR.Emit (emitIR)

import System.Environment
import System.Exit
import Data.Either
import qualified Data.ByteString.Char8 as B
import Debug.Trace

main :: IO ()
main = getArgs >>= parseArgs >>= exitWith

parseArgs :: [String] -> IO ExitCode
parseArgs [] = do
    putStrLn "No input filename given."
    return $ ExitFailure 64
parseArgs [filename] = do
    file <- readFile filename
    let ast = replaceFunc <$> getAST filename file
    case trace (show ast) ast of
      Left pe -> do
          print pe
          return $ ExitFailure 1
      Right prog -> do
          eRes <- IR.Emit.emitIR prog
          B.putStrLn eRes
          return ExitSuccess
parseArgs (x:xs) = putStrLn "Received more than one parameter. Ignoring the rest, considering first one as filename." >> parseArgs [x]