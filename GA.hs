{-# LANGUAGE FlexibleInstances #-}

module GA (
    module GA,
    module System.Random,
    module Control.Monad.State,
    module Control.Monad.Writer,
    module Control.Monad.Reader,
    module Control.Monad.Trans.Except
) where

import Util
import FFNN
import GHC.Word
import Data.Bits
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Either
import Text.Printf
import Unsafe.Coerce
import System.Random
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Except
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

mutateChromosome :: ByteString -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) ByteString
mutateChromosome = fmap pack . sequence . fmap mutateByte . unpack
        where
            mutateByte :: Word8 -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) Word8
            mutateByte byte = let
                                diceRoll = state $ randomR (0, 1)
                                targetBit = state $ randomR (0, 7)
                              in
                                ask >>=
                                (liftM2 (doMutate byte) diceRoll targetBit <*>) . pure
            doMutate :: Word8 -> Float -> Int -> Float -> Word8
            doMutate byte diceRoll targetBit coeff = if diceRoll < coeff
                                                     then complementBit byte targetBit
                                                     else byte

data Citizen a = Citizen {
    getNetwork :: a,
    getDataset :: [([Value],[Value])]
} deriving (Show)

type Genotype a = [a]

sequenceGenotype :: Genotype ByteString -> String
sequenceGenotype = concat . map sequenceGene

mutateGen :: Genotype ByteString -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Genotype ByteString)
mutateGen = sequence . fmap mutateChromosome

crossoverGen :: Genotype ByteString -> Genotype ByteString -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Maybe (Genotype ByteString))
crossoverGen left right = sequence . fmap sequence $ zipWith' chooseChromosome left right
        where
            chooseChromosome :: ByteString -> ByteString -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) ByteString
            chooseChromosome left right = state (randomR (0, 1)) >>=
                                          return . ([left, right] !!)

class HasGenotype a where
    mutate :: a -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Either String a)
    mutate citizen = let
                        mutant = toGenotype citizen >>= return . fmap fromGenotype . sequence . fmap mutateGen
                     in
                        writeLog ("Mutating citizen: " ++ (maybe "<Corpse?>" (sequenceGenotype . snd) $ toGenotype citizen)) >>
                        (fmap (maybe (Left "Failed in mutation genotype conversion") Right . (>>= id)) . sequence $ mutant) >>=
                        traceLogF (("Mutated citizen to: "++) . either (const "<Corpse?>") (maybe "<Corpse?>" (sequenceGenotype . snd) . toGenotype))
    crossover :: a -> a -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Either String a)
    crossover left right = let
                            leftGen = snd <$> toGenotype left
                            rightGen = snd <$> toGenotype right
                            child = liftM2 crossoverGen leftGen rightGen >>=
                                    return . fmap ((fromGenotype . (,) left) =<<)
                           in
                            writeLog ("Breeding citizens: " ++ (maybe "<Corpse?>" sequenceGenotype leftGen) ++ ", " ++ (maybe "<Corpse?>" sequenceGenotype rightGen)) >>
                            (fmap (maybe (Left "Failed in crossover genotype conversion") Right . (>>= id)) . sequence $ child) >>=
                            traceLogF (("Created child: "++) . either (const "<Corpse?>") (maybe "<Corpse?>" (sequenceGenotype . snd) . toGenotype))

    fitness :: a -> Writer [String] Double
    toGenotype :: a -> Maybe (a, Genotype ByteString)
    fromGenotype :: (a, Genotype ByteString) -> Maybe a

instance (Show a, IsGraph a) => HasGenotype (Citizen (Network a)) where
    fitness (Citizen net set) = writeLog ("Valuating network: " ++ (show net)) >>
                                runExceptT (sequence $ map (flip (uncurry evalError) net) set) >>=
                                either (dropWith $ 1 / 0) (return . sum) >>=
                                traceLogF (("Got fitness: "++) . show)
            where
                dropWith :: Double -> String -> Writer [String] Double
                dropWith x e = writeLog ("Dropping citizen: " ++ e) >>
                               pure x
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

evolve :: (Show a, IsGraph a) => Population (Network a) -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Population (Network a))
evolve pop = let
                elite = take 16 $ sortOn fitness pop
             in
                breed elite
        where
            breed :: (Show a, IsGraph a) => Population (Network a) -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Population (Network a))
            breed parents = fmap dropLefts (sequence $ crossover <$> parents <*> parents) >>=
                            fmap ((parents ++) . dropLefts) . sequence . map mutate
            dropLefts :: [Either String a] -> [a]
            dropLefts = rights

takeMax :: (Show a, IsGraph a) => Population (Network a) -> Either String (Citizen (Network a))
takeMax = maybe (Left "Can't sample an empty population") Right . head' . sortOn fitness

seedPopulation :: (Show a, IsGraph a) => Int -> Citizen (Network a) -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Population (Network a))
seedPopulation count seed = writeLog ("Seeding population of " ++ (show count) ++ " from: " ++ (show seed)) >>
                            (fmap catMaybes . sequence $ replicate count (randomize seed))
        where
            randomize :: (Show a, IsGraph a) => Citizen (Network a) -> ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) (Maybe (Citizen (Network a)))
            randomize = fmap (>>= id) . sequence . fmap (fmap fromGenotype . sequence . fmap (sequence . map (const randomBytes))) . toGenotype
            randomBytes :: ExceptT String (StateT StdGen (ReaderT Float (Writer [String]))) ByteString
            randomBytes = fmap pack . sequence . replicate 4 $ state random

