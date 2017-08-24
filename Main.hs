
module Main where

import GA
import FFNN

iterateM :: (Monad m) => (a -> m a) -> a -> m [a]
iterateM f x = f x >>= iterateM f >>= return . (x:)

tryEvolution :: StdGen -> Either String [[Value]]
tryEvolution rng = let
                    trainingSet :: [([Value],[Value])]
                    trainingSet = [([0,0],[1]),([0,1],[0]),([1,0],[0]),([1,1],[1])]
                    patriarch :: Citizen (Network AdjacencyList)
                    patriarch = Citizen createNetwork trainingSet
                   in
                    (takeMax . (!! 100) . evalState (iterateM evolve =<< seedPopulation 256 patriarch)) rng >>=
                    trainNetwork trainingSet . getNetwork >>=
                    sequence . sequence (map (flip runNetwork) (map fst trainingSet)) >>=
                    sequence . map getOutput
        where
            trainNetwork :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> Either String (Network a)
            trainNetwork set net = foldl (>>=) (Right net) (replicate 10 $ flip (trainOn 10) set)
            evalErrors :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> Either String [Value]
            evalErrors set net = sequence $ map (flip (uncurry evalError) net) set

main :: IO ()
main = getStdGen >>=
       putStrLn . either id show . tryEvolution

