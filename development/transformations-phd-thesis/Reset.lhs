\begin{code}
module CommunicationRefinement where

import ForSyDeStdLib

data ReadFIFO = ReadFIFO deriving (Show)
data RecMsg = Ready | Ack deriving (Show)
data SendMsg = DataReady deriving(Show)
data Reset = Reset deriving (Show)

-- finite FIFO

fifoState' q Abst             Abst    = q
fifoState' q Abst            (Prst x) = pushFQ q x
fifoState' q (Prst ReadFIFO)  Abst    = fst (popFQ q)  
fifoState' q (Prst ReadFIFO) (Prst x) = pushFQ (fst (popFQ q)) x

fifoOutput' q          Abst           _ = Abst
fifoOutput' (FQ n [])     (Prst ReadFIFO) _ = Abst
fifoOutput' (FQ n (x:xs)) (Prst ReadFIFO) _ = Prst x

readFIFO' =  mealy2SY fifoState' fifoOutput' (FQ 2 [])


-- ideal FIFO

fifoState q (Prst Reset) _		  _	  = q
fifoState q Abst         Abst             Abst    = q
fifoState q Abst	 Abst            (Prst x) = pushQ q x
fifoState q Abst	 (Prst ReadFIFO)  Abst    = fst (popQ q)  
fifoState q Abst	 (Prst ReadFIFO) (Prst x) = pushQ (fst (popQ q)) x

fifoOutput q         (Prst Reset) _	          _ = Abst
fifoOutput q          Abst	  Abst            _ = Abst
fifoOutput (Q[])      Abst	  (Prst ReadFIFO) _ = Abst
fifoOutput (Q(x:xs))  Abst	  (Prst ReadFIFO) _ = Prst x

readFIFO =  mealy3SY fifoState fifoOutput (Q [])
-- Sender

data SenderStates = ReadFifo
		  | WaitFifo1
		  | WaitFifo2
		  | SendDataReady
		  | WaitReady
		  | SendData
		  | WaitAck     deriving (Show)
sender = moore3SY senderState senderOutput initStateSender 

--	    (State, Value)     (Reset) (fifoOutput) (recMsg)	(nextState, value)
senderState _		       (Prst Reset) _	     _	          = initStateSender
senderState (ReadFifo,v)       Abst	    _	     _		  = (WaitFifo1,v)
senderState (WaitFifo1, v)     Abst	    (Prst x) _		  = (SendDataReady, x)
senderState (WaitFifo1, v)     Abst	    _	_		  = (WaitFifo2,v)
senderState (WaitFifo2, v)     Abst	    (Prst x) _		  = (SendDataReady, x)
senderState (WaitFifo2, v)     Abst	    _	     _		  = (ReadFifo, v)
senderState (SendDataReady, v) Abst	    _	     _	          = (WaitReady, v)
senderState (WaitReady, v)     Abst	    _	     (Prst Ready) = (SendData, v)
senderState (WaitReady, v)     Abst	    _	     _	          = (WaitReady, v)
senderState (SendData, v)      Abst	    _	     _	          = (WaitAck, v)
senderState (WaitAck, v)       Abst	    _	     (Prst Ack)   = (ReadFifo, v)
senderState (WaitAck, v)       Abst	    _	     _	          = (WaitAck, v)

initStateSender = (ReadFifo, 0)

--           (State, Value)	  (sendFifo, sendMsg, sendData)
senderOutput (ReadFifo, _)      = (Prst ReadFIFO, Abst, Abst)
senderOutput (SendDataReady, _) = (Abst, Prst DataReady, Abst)
senderOutput (SendData, x)      = (Abst, Abst, Prst x)
senderOutput (_, _)             = (Abst, Abst, Abst)





-- Receiver

data RecState = WaitDataReady
	      | SendReady
	      | WaitData
	      | SendAck

--       (State, value)     sendMsg          sendData   (nextState, value)
recState _		    (Prst Reset) _                 _	   = initStateReceiver
recState (WaitDataReady, v) Abst	 (Prst DataReady) _        = (SendReady, v)
recState (WaitDataReady, v) Abst	 _		     _        = (WaitDataReady, v)
recState (SendReady, v)     Abst	 _		     _        = (WaitData, v)
recState (WaitData, v)      Abst	 _                (Prst x) = (SendAck, x)
recState (WaitData, v)	    Abst	 _		     Abst     = (WaitData, v)
recState (SendAck, v)	    Abst	 _		     _	      = (WaitDataReady, v)

--        (State, value)    (recMsg, recData)
recOutput (SendReady, _)  = (Prst Ready, Abst)
recOutput (SendAck, v)    = (Prst Ack, Prst v)
recOutput _		  = (Abst, Abst)

receiver = moore3SY recState recOutput initStateReceiver 

initStateReceiver = (WaitDataReady, 0)


-- Handshake Protocol Ideal Fifo

handshake reset xs = holdSY 0 recData
  where 
    senderOut    = sender reset fifoOutput recMsg
    sendFifo     = mapSY fstOf3 senderOut
    sendMsg      = mapSY sndOf3 senderOut
    sendData	 = mapSY thrdOf3 senderOut
    fifoOutput	 = readFIFO reset sendFifo xs
    recOutput	 = receiver reset sendMsg sendData
    recMsg	 = mapSY fst recOutput
    recData	 = mapSY snd recOutput

-- Handshake Protocol Finite Fifo (Size 2)

handshake' xs = holdSY 0 recData
  where 
    senderOut    = sender reset fifoOutput recMsg
    sendFifo     = mapSY fstOf3 senderOut
    sendMsg      = mapSY sndOf3 senderOut
    sendData	 = mapSY thrdOf3 senderOut
    fifoOutput	 = readFIFO' sendFifo xs
    recOutput	 = receiver reset sendMsg sendData
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

reset = signal [Prst Reset, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, 
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst,	
                 Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst, Abst]	
\end{code}