module ServerState 
    (Inmess(..),
	 Outmess(..),
	 ServerState,
	 addToQueue,
	 serverStep,
	 simulationStep,
	 serverStart,
	 serverSize,
	 shortestQueue,
	 joinQueues
	) where
import QueueState 

newtype ServerState = SS  [QueueState] 
                         deriving (Show,Eq)
						 
addToQueue :: Int -> Inmess -> ServerState -> ServerState
addToQueue n im (SS st)	= SS (take n st++[newQueueState]++drop (n+1) st)
  where
  newQueueState = addMessage im (st!!n)  
  
joinQueues :: [Inmess] -> ServerState -> ServerState 
joinQueues imess (SS [QS time served xs]) = SS [QS time served (xs++imess)]
joinQueues imess (SS qs) = SS (zipWith addMessage imess qs)

 
serverStep :: ServerState -> (ServerState, [Outmess])	
serverStep (SS [])
  = (SS [],[])
serverStep (SS (q:qs)) 
  =  (SS (q':qs') , mess++messes)
    where
    (q' , mess)       = queueStep  q
    (SS qs' , messes) = serverStep (SS qs)
	
simulationStep :: ServerState -> [Inmess] -> (ServerState, [Outmess])
simulationStep servSt imess
  = (joinQueues imess servSt1, outmess) 
    where 
    (servSt1, outmess) = serverStep servSt 

addNewObject :: Inmess -> ServerState -> ServerState
addNewObject No servSt = servSt 
addNewObject (Yes arr serv) servSt 
  = addToQueue (shortestQueue servSt) (Yes arr serv) servSt  

serverStart :: ServerState
serverStart = SS (replicate numQueues queueStart)
  where 
  numQueues = 2*2

serverSize :: ServerState -> Int
serverSize (SS xs) = length xs

shortestQueue :: ServerState -> Int
shortestQueue (SS [q]) = 0
shortestQueue (SS (q:qs))
  | (queueLength (qs!!short) <= queueLength q)  = short+1
  | otherwise                                   = 0
    where 
    short = shortestQueue (SS qs)	
	
serverSt1 = SS [(QS 13 4 [Yes 8 4]), (QS 13 3 [Yes 8 4])]
