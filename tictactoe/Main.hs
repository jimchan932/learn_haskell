
import TTT.Game

import Control.Monad.State

getLineReal :: StateT GameState IO String
getLineReal = do
  x <- liftIO $ getLine
  return x 

main = do
  result <- evalStateT play (GameState getLineReal []) 
  case result of 
    ComputerWin -> putStrLn "Computer wins"
    HumanWin -> putStrLn "you won!"
    Draw -> putStrLn "Draw"
    Quit -> putStrLn "goodbye"
