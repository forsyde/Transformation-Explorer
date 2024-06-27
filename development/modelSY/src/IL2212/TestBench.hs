-----------------------------------------------------------------------------
-- |
-- Module      :  IL2212.TestBench
-- Copyright   :  (c) George Ungureanu, 2018
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  stable
-- Portability :  portable
--
-- Contains the testbench of this project.
-----------------------------------------------------------------------------
module IL2212.TestBench (testBench) where

import Data.List (intercalate)
import ForSyDe.Shallow (signal, fromSignal)
import ForSyDe.Shallow.Core.Vector (fromVector, mapV)
import IL2212.ImageProcessing
import IL2212.Utilities

-- | Testbench function for the ForSyDe process network
-- 'IL2212.ImageProcessing.imageProcessing' . It reads a
-- <https://en.wikipedia.org/wiki/Netpbm_format P3 PPM image> as a
-- stream of pixels, replicates it @n@ times, feeds it into the
-- process network, collects the result stream and prints out the
-- output image(s).
testBench :: [FilePath] -- ^ path to the input PPM files.
          -> IO ()
testBench filePaths = do
  (dimX, dimY, imageStream) <- readAllPPM filePaths
  let -------testbench-------
    imageInStream = groupEvery (3 * dimX * dimY) imageStream :: [[Int]]
    matrixImageStream = map (toImage (3*dimX) dimY) imageInStream
    signalOut = imageProcessingSY (signal matrixImageStream)
    -------testbench-------
    imageList = fromSignal signalOut
    in
      putStrLn . intercalate "\n====\n" $ map (intercalate "\n" . fromVector . mapV show) imageList
