import System.IO
import Cookbook.Essential.Continuous
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Functional.Break
import Cookbook.Essential.IO
import System.Environment
import Cookbook.Essential.Common

--intersperse :: [a] -> a -> [a]
--intersperse [] _ = []
--intersperse (x:xs) b = x:b:intersperse xs

rmWs :: String -> String
rmWs = (flip remove) '\t'

rmDb :: (Eq a) => [a] -> [a]
rmDb [] = []
rmDb (x:xs) = x : rmDb (remove xs x)

getContact :: String -> String
getContact x = (rmWs (after x ':'))

main = do
  (file:_) <- getArgs
  flLn <- filelines file
  putStrLn $ flt $rmDb (intersperse (map getContact [c | c <- flLn, c `contains` "Contact:"]) ", ")
  
