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
{- sample
--x is the new string read. y is the internal state
--alternatively, use "StateT IO Int"
step :: String -> Int -> IO Int
step x y = 
    do
      f <- runInterpreter $ defIp >> interpret x (as :: Int -> Int)
      case f of
       Left err -> do
         putStrLn (show err)
         return y
       Right f' -> do
         let s = (f' y)
         putStrLn (show s)
         return s

main = until_ (=="quit") step (readPrompt "> ") 0


-}
