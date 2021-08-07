module Nim where

import Control.Monad (zipWithM_)
import Data.Char (digitToInt, isDigit, ord)

type Board = [Int]

next :: Int -> Int
next 1 = 2
next _ = 1

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [if r == row then n - num else n | (n,r) <- zip board [1..]]

putRow :: Int -> Int -> IO ()
putRow row num = putStrLn $ show row ++ ": " ++ concat (replicate num "* ")

putBoard :: Board -> IO ()
putBoard = zipWithM_ putRow [1..]

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
     then return $ digitToInt x
     else do
       putStrLn "ERROR: Invalid digit"
       getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board
  newline
  if finished board
     then do
       putStr "Player "
       putStr (show (next player))
       putStrLn " wins!!"
     else do
       putStr "Player "
       print player
       row <- getDigit "Enter a row number: "
       num <- getDigit "Stars to remove: "
       if valid board row num
          then play (move board row num) (next player)
          else do
            newline
            putStrLn "ERROR: Invalid move."
            play board player

nim :: IO ()
nim = play initial 1
