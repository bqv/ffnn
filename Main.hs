
module Main where

import GA
import FFNN

trainingSet :: [([Value],[Value])]
trainingSet = [([0,0],[1]),([0,1],[0]),([1,0],[0]),([1,1],[1])]

iterateM :: (Monad m) => (a -> m a) -> a -> m [a]
iterateM f x = f x >>= iterateM f >>= return . (x:)

tryEvolution :: Float -> StdGen -> ExceptT String (Writer [String]) [([Value],[Value])]
tryEvolution coeff rng = let
                            patriarch :: Citizen (Network AdjacencyList)
                            patriarch = Citizen createNetwork trainingSet
                         in
                            ExceptT (flip runReaderT coeff . flip evalStateT rng . runExceptT $ iterateM evolve =<< seedPopulation 256 patriarch) >>=
                            ExceptT . return . takeMax . (!! 10) >>=
                            flip (foldl (>>=)) (replicate 10 $ flip (trainOn 10) trainingSet) . pure . getNetwork >>=
                            mapNetwork (map fst trainingSet)
        where
            evalErrors :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> ExceptT String (Writer [String]) [Value]
            evalErrors set net = sequence $ map (flip (uncurry evalError) net) set

cheat :: ExceptT String (Writer [String]) [([Value],[Value])]
cheat = foldl (>>=) (pure createNetwork) (replicate 10 $ flip (trainOn 10) trainingSet) >>=
        mapNetwork (map fst trainingSet)
        where
            trainNetwork :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> ExceptT String (Writer [String]) (Network a)
            trainNetwork set net = foldl (>>=) (pure net) (replicate 10 $ flip (trainOn 10) set)

main :: IO ()
main = getStdGen >>=
       sequence . map (printLines . runWriter . runExceptT) . sequence [const cheat, tryEvolution 0.3] >>
       return ()
        where
            printLines :: (Show a) => (Either String a, [String]) -> IO ()
            printLines (a, w) = mapM_ (const $ return ()) w >>
                                putStrLn (either id show a)

