\begin{code}
module MapMerge where

import ForSyDeStdLib

-- Transformation BalancedTree

mapMerge f g s = pn_ref f g s
  
-- Original Process

pn_org f g s = (mapSY f . mapSY g) s 

-- Transformed Process

pn_ref f g s = mapSY (f . g) s


s1 = signal [1,2,3,4,5,6]
\end{code}