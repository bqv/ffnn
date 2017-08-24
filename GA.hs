{-# LANGUAGE FlexibleInstances #-}

import FFNN
import GHC.Word
import Data.Bits
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
import Unsafe.Coerce
import System.Random
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.Map.Strict as Map

{- Bits -}

bytesToDouble :: ByteString -> Double
bytesToDouble x = unsafeCoerce (pack64 x)
        where
            pack64 :: ByteString -> Word64
            pack64 x = sum $ zipWith shiftL (map fromIntegral . reverse $ unpack x) [56,48..0]

doubleToBytes :: Double -> ByteString
doubleToBytes x = unpack64 (unsafeCoerce x)
        where
            unpack64 :: Word64 -> ByteString
            unpack64 x = pack . reverse $ map (fromIntegral . (shiftR x)) [56,48..0]

{- Genetics -}

class Chromosome a where
    sequenceGene :: a -> String
    emptyGene :: a

instance Chromosome ByteString where
    sequenceGene = concat . fmap (printf "%02X") . unpack
    emptyGene = pack [0,0,0,0]

mutateChromosome :: ByteString -> State StdGen ByteString
mutateChromosome = fmap pack . sequence . fmap mutateByte . unpack
        where
            mutateByte :: Word8 -> State StdGen Word8
            mutateByte byte = let
                                diceRoll = state $ randomR (0, 1)
                                targetBit = state $ randomR (0, 7)
                              in
                                liftM2 (doMutate byte) diceRoll targetBit
            doMutate :: Word8 -> Float -> Int -> Word8
            doMutate byte diceRoll targetBit = if diceRoll < 0.1
                                               then complementBit byte targetBit
                                               else byte

data Citizen a = Citizen {
    getNetwork :: a,
    getDataset :: [([Value],[Value])]
} deriving (Show)

type Genotype a = [a]

mutateGen :: Genotype ByteString -> State StdGen (Genotype ByteString)
mutateGen = sequence . fmap mutateChromosome

crossoverGen :: Genotype ByteString -> Genotype ByteString -> State StdGen (Maybe (Genotype ByteString))
crossoverGen left right = sequence . fmap sequence $ zipWith' chooseChromosome left right
        where
            chooseChromosome :: ByteString -> ByteString -> State StdGen ByteString
            chooseChromosome left right = state (randomR (0, 1)) >>=
                                          return . ([left, right] !!)

class HasGenotype a where
    mutate :: a -> State StdGen (Maybe a)
    mutate citizen = let
                        mutant = toGenotype citizen >>= return . fmap fromGenotype . sequence . fmap mutateGen
                     in
                        fmap (>>= id) . sequence $ mutant
    crossover :: a -> a -> State StdGen (Maybe a)
    crossover left right = let
                            leftGen = snd <$> toGenotype left
                            rightGen = snd <$> toGenotype right
                            result = liftM2 crossoverGen leftGen rightGen >>=
                                     return . fmap ((fromGenotype . (,) left) =<<)
                           in
                            fmap (>>= id) . sequence $ result

    fitness :: a -> Double
    toGenotype :: a -> Maybe (a, Genotype ByteString)
    fromGenotype :: (a, Genotype ByteString) -> Maybe a

instance (Show a, IsGraph a) => HasGenotype (Citizen (Network a)) where
    fitness (Citizen net set) = maybe (1 / 0) sum . sequence $ map (flip (uncurry evalError) net) set
    toGenotype citizen = let
                            graph = getGraph $ getNetwork citizen
                            layers = getLayers $ getNetwork citizen
                            edges = concatMap concat $ liftM2 (zipWith ((<*>) . fmap (,))) (init' layers) (tail' layers)
                            weights = sequence (map (uncurry getWeight) edges) graph
                            biases = map (flip Map.lookup . getBias . getNetwork $ citizen) (listNodes graph)
                            bytes = map (maybe emptyGene doubleToBytes) (weights ++ biases)
                         in
                            return (citizen, bytes)
    fromGenotype (cz, bytes) = let
                                network = getNetwork cz
                                graph = getGraph network
                                biasmap = getBias network
                                layers = getLayers network
                                edges = concatMap concat $ liftM2 (zipWith ((<*>) . fmap (,))) (init' layers) (tail' layers)
                                fweights = zipWith' ($) (map (uncurry setWeight) edges)
                                fbiases = zipWith' ($) (map Map.insert $ listNodes graph)
                                nnodes = length $ listNodes graph
                                (weights, biases) = splitAt (length edges) . map bytesToDouble $ bytes
                                newgraph = flip (foldl (.) id) graph <$> fweights weights
                                newbiases = flip (foldl (.) id) biasmap <$> fbiases biases
                               in
                                liftA2 (updateNetwork network) newgraph newbiases >>=
                                return . updateCitizen cz
        where
            updateNetwork net graph bias = net { getGraph = graph, getBias = bias }
            updateCitizen citizen network = citizen { getNetwork = network }

type Population a = [Citizen a]

evolve :: (Show a, IsGraph a) => Population (Network a) -> State StdGen (Population (Network a))
evolve pop = let
                elite = take 16 $ sortOn fitness pop
             in
                breed elite
        where
            breed :: (Show a, IsGraph a) => Population (Network a) -> State StdGen (Population (Network a))
            breed parents = fmap catMaybes (sequence $ crossover <$> parents <*> parents) >>=
                            fmap ((parents ++) . catMaybes) . sequence . map mutate

takeMax :: (Show a, IsGraph a) => Population (Network a) -> Maybe (Citizen (Network a))
takeMax = head' . sortOn fitness

{- Code -}

seedPopulation :: (Show a, IsGraph a) => Int -> Citizen (Network a) -> State StdGen (Population (Network a))
seedPopulation count seed = fmap catMaybes . sequence $ replicate count (randomize seed)
        where
            randomize :: (Show a, IsGraph a) => Citizen (Network a) -> State StdGen (Maybe (Citizen (Network a)))
            randomize = fmap (>>= id) . sequence . fmap (fmap fromGenotype . sequence . fmap (sequence . map (const randomBytes))) . toGenotype
            randomBytes :: State StdGen ByteString
            randomBytes = fmap pack . sequence . replicate 4 $ state random

trainOn :: (Show a, IsGraph a) => Int -> Network a -> [([Value],[Value])] -> Maybe (Network a)
trainOn n network dataset = foldl (>>=) (Just network) $ map (trainN n) dataset
        where
            trainN :: (Show a, IsGraph a) => Int -> ([Value],[Value]) -> Network a -> Maybe (Network a)
            trainN 0 input net = Just net
            trainN n input net = uncurry learnFrom input net >>=
                                 trainN (n-1) input

mapNetwork :: (Show a, IsGraph a) => [[Value]] -> Network a -> [([Value],[Value])]
mapNetwork inputs net = let
                            outputs :: [[Value]]
                            outputs = fromJust . sequence $ map ((>>= getOutput) . runNetwork net) inputs
                        in
                            zip inputs outputs

iterateM :: (Monad m) => (a -> m a) -> a -> m [a]
iterateM f x = f x >>= iterateM f >>= return . (x:)

main :: IO ()
main = let
        trainingSet :: [([Value],[Value])]
        trainingSet = [([-1,-1],[1]),([-1,1],[-1]),([1,-1],[-1]),([1,1],[1])]
        trainedNetwork :: Network AdjacencyList
        trainedNetwork = trainNetwork trainingSet createNetwork
       in
        let
            citizen = Citizen createNetwork trainingSet
        in
            getStdGen >>=
            return . fromJust . takeMax . (!! 100) . evalState (iterateM evolve =<< seedPopulation 256 citizen) >>=
            putStrLn . show . fmap (>>= getOutput) . sequence (map (flip runNetwork) (map fst trainingSet)) . getNetwork        where
            trainNetwork :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> Network a
            trainNetwork set net = fromJust $ foldl (>>=) (Just net) (replicate 10 $ flip (trainOn 10) set)
            evalErrors :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> [Value]
            evalErrors set net = fromJust . sequence $ map (flip (uncurry evalError) net) set

