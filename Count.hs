module Count (counts
             ,topX
             ,showPair) where
import System.IO (putStrLn)
import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map

removePunc :: [String] -> [String]
removePunc = map (filter (not . isPunctuation))

counts :: String -> [(String, Int)]
counts x = Map.toList $ Map.fromListWith (+) (makeKVPairs x)

makeKVPairs :: String -> [(String, Int)]
makeKVPairs x = zip (removePunc $ words $ map toLower x) (repeat 1)

showPair :: (String, Int) -> IO ()
showPair (x, y) = putStrLn $ "(" ++ show x ++ ", " ++ show y ++ ")"

topX :: [(String, Int)] -> Int -> [(String, Int)]
topX x n = take n $ sortBy (flip (compare `on` snd)) x
