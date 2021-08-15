module TicTacToe where

import Data.Char
import Data.List
import System.IO
import Ch10 (cls, goto)


type Grid = [[Player]]
data Player = O | B | X deriving (Eq, Ord, Show)
data Tree a = Node a [Tree a] deriving Show

size :: Int
size = 3

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (notElem B)

turn :: Grid -> Player
turn g = if xs <= os
            then X
            else O
  where
    os = length $ filter (==O) ps
    xs = length $ filter (==X) ps
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (==p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . intercalate bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . intersperse bar . map showPlayer
  where beside = foldr1 $ zipWith (++)
        bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = [chop size (xs ++ [p] ++ ys) | valid g i]
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
     then return (read xs)
     else do
       putStrLn "ERROR: Invalid number."
       getNat prompt

tictactoe :: IO ()
tictactoe = run empty X

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1,1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do
           i <- getNat (prompt p)
           case move g i p of
                [] -> do
                  putStrLn "ERROR: Invalid move"
                  run' g p
                [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
