\begin{code}
module CommunicationRefinement where

import ForSyDeStdLib

data ReadFIFO = ReadFIFO deriving (Show)
data RecMsg = Ready | Ack deriving (Show)
data SendMsg = DataReady deriving(Show)

-- finite FIFO

fifoState' q Abst             Abst    = q
fifoState' q Abst            (Prst x) = pushFQ q x
fifoState' q (Prst ReadFIFO)  Abst    = fst (popFQ q)  
fifoState' q (Prst ReadFIFO) (Prst x) = pushFQ (fst (popFQ q)) x

fifoOutput' q              Abst           _ = Abst
fifoOutput' (FQ n [])     (Prst ReadFIFO) _ = Abst
fifoOutput' (FQ n (x:xs)) (Prst ReadFIFO) _ = Prst x

readFIFO' =  mealy2SY fifoState' fifoOutput' (FQ 2 [])


-- ideal FIFO

fifoState q Abst             Abst    = q
fifoState q Abst            (Prst x) = pushQ q x
fifoState q (Prst ReadFIFO)  Abst    = fst (popQ q)  
fifoState q (Prst ReadFIFO) (Prst x) = pushQ (fst (popQ q)) x

fifoOutput q          Abst           _ = Abst
fifoOutput (Q[])     (Prst ReadFIFO) _ = Abst
fifoOutput (Q(x:xs)) (Prst ReadFIFO) _ = Prst x

readFIFO =  mealy2SY fifoState fifoOutput (Q [])
-- Sender

data SenderStates = ReadFifo
		  | WaitFifo1
		  | WaitFifo2
		  | SendDataReady
		  | SendData	deriving (Show)
 

--	    (State, Value)     (fifoOutput) (recMsg)	(nextState, value)
senderState (ReadFifo,v)       _        _            = (WaitFifo1,v)
senderState (WaitFifo1, v)     (Prst x) _	     = (SendDataReady, x)
senderState (WaitFifo1, v)     _	_	     = (WaitFifo2,v)
senderState (WaitFifo2, v)     (Prst x) _	     = (SendDataReady, x)
senderState (WaitFifo2, v)     _	_	     = (ReadFifo, v)
senderState (SendDataReady, v) _	(Prst Ready) = (SendData, v)
senderState (SendDataReady, v) _	_	     = (SendDataReady, v)
senderState (SendData, v)      _	(Prst Ack)   = (ReadFifo, v)
senderState (SendData, v)      _	_	     = (SendData, v)

--           (State, Value)	  (sendFifo, sendMsg, sendData)
senderOutput (ReadFifo, _)      = (Prst ReadFIFO, Abst, Abst)
senderOutput (SendDataReady, _) = (Abst, Prst DataReady, Abst)
senderOutput (SendData, x)      = (Abst, Abst, Prst x)
senderOutput (_, _)             = (Abst, Abst, Abst)

sender = moore2SY senderState senderOutput initState
   where initState = (ReadFifo, 0)


-- Receiver

data RecState = WaitDataReady
	      | WaitData
	      | OutputData

--       (State, value)     sendMsg          sendData   (nextState, value)
recState (WaitDataReady, v) (Prst DataReady)  _        = (WaitData, v)
recState (WaitDataReady, v)  Abst	      _	       = (WaitDataReady, v)
recState (WaitData, v)	     _		     (Prst x)  = (OutputData, x)
recState (WaitData, v)	     _		      Abst     = (WaitData, v)
recState (OutputData, v)     _		      _	       = (WaitDataReady, v)

--        (State, value)    (recMsg, recData)
recOutput (WaitDataReady, _) = (Prst Ack, Abst)
recOutput (WaitData, _)      = (Prst Ready, Abst)
recOutput (OutputData, v)    = (Abst, Prst v)

receiver = moore2SY recState recOutput initState 
    where initState = (WaitDataReady, 0)


-- Handshake Protocol Ideal Fifo

handshake xs = recData
  where 
    senderOut    = delaySY (Abst, Abst, Abst) (sender fifoOutput recMsg)
    sendFifo     = mapSY fstOf3 senderOut
    sendMsg      = mapSY sndOf3 senderOut
    sendData	 = mapSY thrdOf3 senderOut
    fifoOutput	 = readFIFO sendFifo xs
    recOutput	 = receiver sendMsg sendData
    recMsg	 = mapSY fst recOutput
    recData	 = mapSY snd recOutput

-- Handshake Protocol Finite Fifo (Size 2)

handshake' xs = recData
  where 
    senderOut    = delaySY (Abst, Abst, Abst) (sender fifoOutput recMsg)
    sendFifo     = mapSY fstOf3 senderOut
    sendMsg      = mapSY sndOf3 senderOut
    sendData	 = mapSY thrdOf3 senderOut
    fifoOutput	 = readFIFO' sendFifo xs
    recOutput	 = receiver sendMsg sendData
    recMsg	 = mapSY fst recOutput
    recData	 = mapSY snd recOutput

fstOf3 (a,b,c)  = a
sndOf3 (a,b,c)  = b
thrdOf3 (a,b,c) = c

sparse = signal [Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
		 Prst 1,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
		 Prst 2,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
		 Prst 3, 
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
		 Prst 4,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst]

dense = signal [Abst, Abst, Prst 1, Abst, Prst 2, Abst, Prst 3, Prst 4, Abst, 
	        Prst 5, Abst, Abst, Abst, Abst, Abst, 
		Abst, Abst, Abst, Abst, Prst 6,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
		 Prst 7, 
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
		 Prst 8,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst]
\end{code}