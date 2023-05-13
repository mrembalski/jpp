import Control.Monad
import Interpreter (evalMain, interpret)
import Ruskell.Par (myLexer, pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

run :: String -> IO ()
run code = case pProgram $ myLexer code of
  Left e -> hPutStrLn stderr e >> exitFailure
  Right parsed -> case interpret parsed of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right interpreted -> case evalMain interpreted of
      Left e -> hPutStrLn stderr e >> exitFailure
      Right val -> print val

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run
    file : xs -> readFile file >>= run
