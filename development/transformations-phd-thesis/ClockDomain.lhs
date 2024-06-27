\begin{code}
module ClockDomain where

import ForSyDeStdLib 

-- groupSamples k = mooreSY f g s0 
--   where
--     s0 = NullV
--     f v x | lengthV v == 0 = unitV x
-- 	  | lengthV v == k = unitV x 
-- 	  | otherwise      = v <: x
--     g v   | lengthV v == 0 = Prst NullV
--     g v   | lengthV v == k = Prst v
--     g v   | otherwise      = Abst



sumV v = foldlV (+) 0 v

pn_org k = mapSY (psi sumV) . groupSY k

groupToMultiRate p k = pn_ref
  where
    pn_ref = upDI k . mapSY (psi p) . downDI k . groupSY k

pn_ref k = groupToMultiRate sumV k

s = signal [1,2,3,4,5,6,7,8,9,10] 
\end{code}
