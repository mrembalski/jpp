import Interpreter (evalMain, interpret)
import Ruskell.Par (myLexer, pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure)

runFile :: FilePath -> IO ()
runFile f = readFile f >>= runProgram

runProgram :: String -> IO ()
runProgram code = case pProgram $ myLexer code of
  Left e -> putStrLn ("parsing: " ++ e) >> exitFailure
  Right parsed -> case interpret parsed of
    Left e -> putStrLn ("interpretation: " ++ e) >> exitFailure
    Right interpreted -> case evalMain interpreted of
      Left e -> putStrLn ("main evaluation: " ++ e) >> exitFailure
      Right val -> print val

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "reading from stdin...\n" >> getContents >>= runProgram
    filename -> mapM_ runFile filename
