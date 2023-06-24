module TTT.Game (
  Player(..)
 ,GameState(..)
 ,GameStatus(..)
 ,render
 ,play
) where

import Control.Monad.State
import TTT.Position
import Data.List.Split
import Data.List
import Data.Char 

import TTT.Engine
import qualified Data.Map as M

data Player = Computer | Human deriving Eq
data GameState = GameState (StateT GameState IO String) [String]
data GameStatus = Quit | ComputerWin | HumanWin | Draw

render (Position board turn) = 
  unlines . intersperse (replicate (dim*3+2) '-') . map (intercalate "|")  . 
  chunksOf dim . map (pad . display) $ zip [0..] board
  where display (i,' ') = intToDigit i 
        display (_,c)   = c 
        pad c = ' ':c:" "

getLine' = do 
  GameState fn x <- get
  fn
 
askForPlayer = do 
  mapM_ (liftIO . putStrLn) 
          [ "Who do you want to go first?" 
          , "  1. Computer"
  	  , "  2. Human"
          , "  q - quit" 
          ]
  input <- getLine'
  case input of 
    "q" -> return Nothing
    "1" -> return $ Just Computer  
    "2" -> return $ Just Human 
    _   -> askForPlayer

askForMove position@(Position board turn) = do 
  liftIO $ putStrLn "Make a move" 
  input <- getLine' 
  case input of 
    "q" -> return Nothing
    [c] | isDigit c || c `elem` "abcdef" -> do 
        let idx = digitToInt c 
        if 0 <= idx && size > idx && ' ' == board !! idx
           then return $ Just idx 
           else do
             liftIO $ putStrLn "Pick an empty square on the board" 
             askForMove position
    _ -> do 
      liftIO $ putStrLn "invalid input.  Please Make a move"
      askForMove position
 
play = do
  liftIO $ putStrLn "Welcome to Tic Tac Toe" 
  maybePlayer <- askForPlayer
  case maybePlayer of 
    Nothing -> return Quit
    Just player -> mainLoop player initPosition 

mainLoop player position = do 
  liftIO $ putStrLn (render position)
  case () of 
    () | [] == possibleMoves position -> return Draw
       | position `isWinFor` 'X' || position `isWinFor` 'O'
         -> if Computer == player
               then return HumanWin 
               else return ComputerWin
       | Human == player -> do
         maybeIdx <- askForMove position
         case maybeIdx of
           Nothing -> return Quit
           Just idx -> mainLoop Computer (move position idx) 
       | otherwise -> mainLoop Human (move position (evalState (bestMove position) M.empty)) 
                       

