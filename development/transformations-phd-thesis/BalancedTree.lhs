\begin{code}
module BalancedTree where

import ForSyDe.Shallow

-- Transformation BalancedTree

balancedTree op vs = pn_ref_bt op vs
  
-- Original Process

pn_org_bt op vs = zipWithxSY (makeFunc op) vs
  where
     makeFunc op (x:> NullV) = x 
     makeFunc op (x:>xv)     = op x (makeFunc op xv)

-- Transformed Process

pn_ref_bt op vs | powerOf2 m = balance op m vs
	        | otherwise  = error "Balanced Tree: Transformation not possible"
   where m = lengthV vs
	 half_m = div m 2 
	 upper  = takeV half_m vs
	 lower  = takeV half_m vs
	 balance op 2 (v1:>v2:>NullV ) = zipWithSY op v1 v2
	 balance op m vs	       = zipWithSY op (balance op half_m upper) 
					              (balance op half_m lower)



-----------------------------------------------------------------------------

-- Transformation Pipelined Tree

--   Format fs : [f7,f6,f5,f4,f3,f2,f1]
--
--   -
--     f1 -
--   -
--          f5 -
--   -
--     f2 -
--   -
--               f7 -             
--   -
--     f3 -
--   -
--          f6 -
--   -
--     f4 -
--   -

pipelinedTree s_0 fs vs = pn_ref_pt s_0 fs vs

-- Original Process
pn_org_pt fs vs = balance' fs m vs
   where m = lengthV vs 

balance' (f:fs) 2 (v1:>v2:>NullV) = zipWithSY f v1 v2
balance' (f:fs) m  vs  = zipWithSY f (balance' (upperNet fs) half_m upper)
				     (balance' (lowerNet fs) half_m lower)
        where half_m = div m 2 
              upper  = takeV half_m vs
	      lower  = takeV half_m vs

-- Transformed Process

pn_ref_pt s_0 fs vs | powerOf2 m = pipelineBalanced' s_0 fs m vs
		    | otherwise  = error "Pipelined Tree: Transformation not possible"
   where m = lengthV vs
 
pipelineBalanced' s_0 (f:fs) 2 (v1:>v2:>NullV) = delaySY s_0 (zipWithSY f v1 v2)
pipelineBalanced' s_0 (f:fs) m vs 
    = delaySY s_0 (zipWithSY f (pipelineBalanced' s_0 (upperNet fs) half_m upper)
		(pipelineBalanced' s_0 (lowerNet fs) half_m lower))
        where half_m = div m 2 
              upper  = takeV half_m vs
	      lower  = takeV half_m vs

-----------------------------------------------------------------------------

-- Transformation Balanced Pipelined Tree

balancedPipelinedTree s_0 f vs = pn_ref_bpt s_0 f vs

-- Original Process

pn_org_bpt f vs = zipWithxSY (makeFunc f) vs  
  where
     makeFunc f (x:> NullV) = x 
     makeFunc f (x:>xv)	    = f x (makeFunc f xv)

-- Transformed Process

pn_ref_bpt s_0 f vs = pipelinedTree s_0 (copy m f) vs
   where m = lengthV vs

-- Auxiliary Functions 

powerOf2 m | m <= 1 = False
           | m == 2 = True
	   | otherwise = powerOf2 (div m 2)

lowerNet fs = lowerNet' 1 fs
  where
    lowerNet' n [] = []
    lowerNet' n fs = take n (drop n fs) ++ lowerNet' (2*n) (drop (2*n) fs)

upperNet fs = upperNet' 1 fs
  where 
    upperNet' n [] = []
    upperNet' n fs = take n fs ++ upperNet' (2*n) (drop (2*n) fs)

copy 0 x = []
copy n x = x : copy (n-1) x

-- Test Signals

fs1 = [(*),(+),(+)]
fs2 = [(+), (+), (+)]    
s = signal [1,2,3,4,5]
vs3 = vector [s,s,s]
vs4 = vector [s,s,s,s]



\end{code}
