module Ch10 where

import qualified Hangman as H
import Control.Monad (replicateM)

run = H.run

pStr :: String -> IO ()
pStr = mapM_ putChar

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- read <$> getLine
  xs <- replicateM n (putStr "? " >> read <$> getLine)
  putStrLn $ "Total: " ++ show (sum xs)

readLine :: IO String
readLine =
  readLine' ""
  where readLine' s = do
          c <- H.getCh
          case c of
               '\DEL' -> do
                 putStr "\b \b"
                 readLine' (init s)
               '\n' -> do
                 putChar c
                 pure s
               _ -> do
                 putChar c
                 readLine' (s ++ [c])
