import qualified Count as C
import System.IO
import Control.Monad (mapM_)

freq :: [(String, Int)] -> [(String, Double)]
freq pairs = let c = fromIntegral $ foldr (\(x,y) acc -> acc + y) 0 pairs
             in map (\(s,y) -> (s, (fromIntegral y)/c)) pairs

main = do
  h <- openFile "Genesis.txt" ReadMode
  contents <- hGetContents h
  mapM_ (putStrLn . show) $ freq $ flip C.topX 100 (C.counts contents)
