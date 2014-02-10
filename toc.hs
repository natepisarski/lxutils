import qualified Cookbook.Essential.IO         as CIO
import qualified Cookbook.Essential.Common     as Cm
import qualified Cookbook.Essential.Continuous as Ct

import qualified Cookbook.Ingredients.Lists.Access as Ac

import System.IO
import System.Environment

parse :: String -> String
parse x
  | and ['#' `elem` x, Ac.count x '#' == 1] = "* "++(Ct.after x '#')
  | '#' `elem` x = (take (2*(Ac.count x '#')) (repeat ' ')++"* ") ++ Cm.fromLast ((flip Ct.before) '#') x
  | otherwise = ""
                
main = do
  flines <- fmap head getArgs >>= CIO.filelines
  mapM_ putStrLn $ "#Contents":(Ct.remove (map parse (map ((flip Ct.remove) "\\#") flines)) [""])
