\begin{code}
module SerialClockDomain where

import ForSyDe.Shallow

-- Original Process
pn_org hs gs = zipWithxSY f 
   where f = makeFunc hs gs 
         -- Example:
         -- f = (+)_4
         --   = (+) (id v3, (+) (id v_2, (+) (id v_1, id v_0)))
         --   = g2  (h3(v3), g1 (h2(v2), g0  (h3 v1, h3 v_0)))
         -- Thus it can transform more functions then a normal reduceV
         -- For f' = reduceV f:
         --      g = replicate (m-1) f
         --      h = replicate  m id
         
-- Transformed Process
pn_ref hs gs s_0 = (downDI m . p_fsm hs gs s_0 . par2serxDI)
   where
     p_fsm hs gs s_0 = mooreSY u w s_0
     u (k,s) x  | k == 0       = (1, h(k) x)
	        | 0 < k && k < m-1 = (k+1, g(k) (h(k) x) s)
		| k == m-1     = (0, g(k) (h(k) x) s)
     w (k,s)    | k == 0       = Prst s
		| 0 < k && k < m = Abst  
     h(k) = hs !! k
     g(k) = gs !! (k-1)
     m = length hs

serialClockDomain hs gs s_0 = pn_ref hs gs s_0



-- Adder Example
adder_org = zipWithxSY (makeFunc hs gs)
  where 
     hs = [id, id, id, id] 
     gs = [(+), (+), (+)]

adder_ref = serialClockDomain hs gs s0
  where
     hs = [id, id, id, id] 
     gs = [(+), (+), (+)]
     s0 = (0, 0)

-- FIR-Example
fir_org = pn_org hs gs 
  where hs = map (*) hlist
	gs = [(+),(+),(+)]
	hlist = [0,1,1,1]

fir_ref = pn_ref hs gs s0
  where hs = map (*) hlist
	gs = [(+),(+),(+)]
	hlist = [0,1,1,1]
	s0 = (0, 0)

-- Auxiliary Function
makeFunc [h] [] (x:>NullV) = h x 
makeFunc (h:hs) (g:gs) (x:>xv) = g (h x) (makeFunc hs gs xv)

-- Test Vector
vs = vector [signal[1,2,3,4,5], signal[4,5,6,7,8], signal[7,8,9,10,11], signal[10,11,12,13,14]]
\end{code}

\begin{code}
-- reduceV Example (Paper)
f_paper = (+)
m_paper = 4
init_Value = 0 -- Identity Element
 
pn_org_paper :: Vector (Signal Integer) -> Signal Integer
pn_org_paper = zipWithxSY (reduceV (f_paper))

pn_ref_paper :: Vector (Signal Integer) -> Signal (AbstExt Integer)
pn_ref_paper = pn_ref hs gs s_0
   where hs = replicate m_paper id
         gs = replicate (m_paper-1) f_paper
         s_0 = (0, init_Value)
\end{code}
