
import Wavelets
import System.Random

main :: IO()
main = do

  let x = take 128 $ randomRs (-1,  1) $ mkStdGen 1 :: [Double]
  print $ dwt 4 haar wp_separate x
