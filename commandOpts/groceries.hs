{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Groceries where 

import Control.Monad
import GetOptFu

data Flag = Store String
    	  | Customer String
 	  | PeanutButter
	  | Jelly
	  | Bananas (Maybe String)
 	  | ExpressLane
          | Help 
 	  | Debug
 	  deriving (Eq, Ord, Show, Read, Typeable, Data)

options :: [OptDescr Flag]
options = [ 
 	Option "s" ["store"] (ReqArg Store "<name>") "Specify a store name",
	Option "c" ["customer"] (ReqArg Customer "<name>") "Specify a customer name",
	Option "p" ["peanutbutter"] (NoArg PeanutButter) "Itemize peanut butter",
	Option "j" ["jelly"] (NoArg Jelly) "Itemize jelly",
	Option "b" ["bananas"] (OptArg Bananas "<quantity>") "Itemize one or more bananas",
	Option "e" ["expresslane"] (NoArg ExpressLane) "Use the express lane",
	Option "h" ["help"] (NoArg Help) "Show usage information",
	Option "d" ["debug"] (NoArg Debug) "Show raw and parsed arguments"
	]

main :: IO ()
main = do 
        program <- getProgName
        args <- getArgs
        pArgs <- parseOpts program args options 
        let debugMode = getOption pArgs Debug == Just Debug
	when debugMode
		(do
			putStrLn $ "Raw Args: " ++ show args
			putStrLn $ "Parsed Args: " ++ show pArgs)
       
