module Hangman where

import System.IO

hangman :: IO ()
hangman = do
  putStrLn "Player 1> Enter a word:"
  w <- sgetLine
  putStrLn "Player 2> Enter your guess."
  play w

sgetLine :: IO String
sgetLine = do
  s <- go []
  return $ reverse s
  where go acc = do
          c <- getCh
          if c == '\n'
             then do
               putChar c
               return acc
             else do
               putChar '-'
               go (c:acc)

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
     then putStrLn "Congratulations! You guessed the word."
     else do
       putStrLn (match word guess)
       play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

run = hangman
