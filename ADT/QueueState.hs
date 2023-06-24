module QueueState 
    (QueueState(..),
	 Inmess(..) ,
	 Outmess(..) ,
	 addMessage,
     queueStep,
     queueStart,
	 queueLength,
	 queueEmpty,
	 ) where

data Inmess = No | Yes Arrival Service deriving (Eq,Show)
type Arrival = Integer  
type Service = Integer
		
data Outmess = Discharge Arrival Wait Service deriving (Eq,Show)
type Wait = Integer 
	 
type Time = Integer 	 
data QueueState = QS Time Service [Inmess]
                  deriving (Eq,Show)

addMessage :: Inmess -> QueueState -> QueueState
addMessage mess (QS time serv ml) = QS time serv (ml++[mess])				  

queueStep :: QueueState -> (QueueState,[Outmess]) 
queueStep (QS time servSoFar (Yes arr serv : inRest))
  | servSoFar < serv 
    = (QS (time+1) (servSoFar+1) (Yes arr serv: inRest), [])
  | otherwise 
    = (QS (time+1) 0 inRest, [Discharge arr (time-serv-arr) serv])
queueStep (QS time serv []) = (QS (time+1) serv [],[]) 

queueStart :: QueueState
queueStart = QS 0 0 []

queueLength :: QueueState -> Int
queueLength (QS _ _ q) = length q

queueEmpty :: QueueState -> Bool
queueEmpty (QS _ _ q) = q==[]

prop_step :: QueueState -> Bool
prop_step q@(QS time served (Yes arr serv:xs)) 
  = queueStep q == (QS (time+1) (served+1) (Yes arr serv:xs),[]) 
	  || queueStep q == (QS (time+1) 0 xs, [Discharge arr (time-serv-arr) serv])     
	  