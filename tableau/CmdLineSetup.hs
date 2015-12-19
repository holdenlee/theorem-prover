module CmdLineSetup where

import System.IO
import Language.Haskell.Interpreter

--Print a String to IO, IMMEDIATELY
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

--Ask for a String with the prompt
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

--Until pred is true, get "m a", carry out the action
until_ :: Monad m => (a -> Bool) -> (a -> b -> m b) -> m a -> b -> m b
until_ pred f ma start = do
  a <- ma
  if pred a
     then return start
     else (f a start) >>= until_ pred f ma 

defIp :: InterpreterT IO ()
defIp = setImportsQ [("Prelude", Nothing)]
                     --("Data.Map", Just "M")
