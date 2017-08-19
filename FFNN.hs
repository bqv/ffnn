{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Applicative
import Debug.Trace

{- Types -}

type Weight = Double
type Value = Double

{- Graphs -}

type NodeMap = Map.Map Int Value

data AdjacencyList = Adjacency (Map.Map (Int, Int) Weight) NodeMap deriving (Show)
data Matrix = Matrix ([[Maybe Weight]]) NodeMap deriving (Show)

class IsGraph a where
    getWeight :: Int -> Int -> a -> Maybe Weight
    getValue :: Int -> a -> Maybe Value
    setWeight :: Int -> Int -> Weight -> a -> a
    setValue :: Int -> Value -> a -> a
    linksFrom :: Int -> a -> [Int]

instance IsGraph AdjacencyList where
    getWeight from to (Adjacency graph _) = Map.lookup (from, to) graph
    getValue node (Adjacency _ nodes) = Map.lookup node nodes 
    setWeight from to weight (Adjacency graph nodes) = let
                                                        newGraph = Map.insert (from, to) weight graph
                                                       in
                                                        Adjacency newGraph nodes
    setValue node val (Adjacency graph nodes) = let
                                                    newNodes = Map.insert node val nodes
                                                in
                                                    Adjacency graph newNodes
    linksFrom node (Adjacency graph _) = map snd . Map.keys . Map.filterWithKey (\k _ -> fst k == node) $ graph

instance IsGraph Matrix where
    getWeight from to (Matrix graph _) = graph ^? element from . element to >>= id
    getValue node (Matrix _ nodes) = Map.lookup node nodes 
    setWeight from to weight (Matrix graph nodes) = let
                                                        newGraph = (element from . element to .~ Just weight) graph 
                                                    in
                                                        Matrix newGraph nodes
    setValue node val (Matrix graph nodes) = let
                                                newNodes = Map.insert node val nodes
                                             in
                                                Matrix graph newNodes
    linksFrom node (Matrix graph _) = concat $ graph ^? element node >>= return . findIndices isJust

{- Networks -}

type Layer = [Int]
type Layers = [Layer]
type NonLinearity = (Value -> Value, Value -> Value) -- (Function, Derivative)

instance Show (Double -> Double) where
    show fn = "<Function>"

data Network g = Network {
    getGraph :: g,
    getLayers :: Layers,
    getNonlin :: NonLinearity,
    getAlpha :: Value,
    getBias :: NodeMap
} deriving (Show)

updateGraph :: Network a -> a -> Network a
updateGraph network graph = network { getGraph = graph }

class Neural a where
    getOutput :: a -> Maybe [Value]
    runNetwork :: a -> [Value] -> Maybe a
    trainNetwork :: a -> [Value] -> Maybe a
    learnFrom :: [Value] -> [Value] -> a -> Maybe a
    evalError :: [Value] -> [Value] -> a -> Maybe Value

instance (Show a, IsGraph a) => Neural (Network a) where
    getOutput network = last' (getLayers network) >>=
                        sequence . map (flip getValue (getGraph network))
    runNetwork network input = let
                                layers = getLayers network
                                biases = head' layers >>=
                                         return . map (flip (Map.findWithDefault 0) $ getBias network)
                                biasedInputs = biases >>= zipWith' ($) (map (+) input)
                                insertions = fmap (zipWith' setValue) (head' layers) <*> biasedInputs >>= id
                                inputGraph = flip (foldl (.) id) (getGraph network) <$> insertions
                               in
                                inputGraph >>=
                                go layers >>=
                                return . updateGraph network
        where
            {- Flow layer x to y -}
            go :: (Show a, IsGraph a) => Layers -> a -> Maybe a
            go (x:y:ys) g = let
                                biases = map (flip (Map.findWithDefault 0) $ getBias network) y
                            in
                                runLayer x y g biases (getNonlin network) >>=
                                go (y:ys)
            go _ g = Just g
    trainNetwork network predic = let
                                    reverseLayers = reverse $ getLayers network
                                    output = head' reverseLayers >>=
                                             sequence . map (flip getValue (getGraph network))
                                    errorl = output >>=
                                             zipWith' findErr predic
                                                where
                                                    findErr ex out = (ex - out) * (($ out) . snd $ getNonlin network)
                                    insertions = fmap (zipWith' Map.insert) (head' reverseLayers) <*> errorl >>= id
                                    errMap = flip (foldl (.) id) Map.empty <$> insertions >>= calcErr reverseLayers
                                    errList = fmap (flip map reverseLayers . map . flip Map.lookup) errMap
                                  in
                                    (sequence . map sequence) <$> errList >>=
                                    liftA2 (mapErr $ getGraph network) (tail' reverseLayers) >>=
                                    fmap reverse >>=
                                    applyWeights >>=
                                    return . updateGraph network
        where
            calcErr :: Layers -> NodeMap -> Maybe NodeMap
            calcErr (x:y:ys) errMap = backpropogate x y (getGraph network) (getNonlin network) errMap >>=
                                      calcErr (y:ys)
            calcErr _ m = Just m
            mapErr g b@(y:ys) c@(e:es) = let
                                        vals :: Maybe [Value]
                                        vals = sequence $ map (flip getValue g) y
                                        links :: [[Int]]
                                        links = map (flip linksFrom g) y
                                        oldWeights :: Maybe [[Weight]]
                                        oldWeights = zipWith' findWeights y links >>= sequence
                                            where
                                                findWeights i os = sequence $ map (flip (getWeight i) g) os
                                        newWeights :: Maybe [[Weight]]
                                        newWeights = liftA2 (adjustWeights e) oldWeights vals >>= id
                                       in
                                        liftA2 (:) newWeights (mapErr g (ys) es)
            mapErr _ b c = Just []
            adjustWeights errors weights inputs = zipWith' (adjustWeights' errors) weights inputs >>= sequence
            adjustWeights' errors weights input = zipWith' (adjustWeights'' input) errors weights
            adjustWeights'' input error weight = let
                                                    alpha = getAlpha network
                                                 in
                                                    weight - (alpha * error * input)
            applyWeights weights = let
                                    layers = getLayers network
                                   in
                                    applyWeights' weights layers >>=
                                    return . flip (foldl (.) id) (getGraph network)
            applyWeights' :: (IsGraph a) => [[[Weight]]] -> Layers -> Maybe [a -> a]
            applyWeights' (w:ws) (x:y:ys) = liftA2 (++) (applyWeights'' w x y) (applyWeights' ws (y:ys))
            applyWeights' [] [x] = Just []
            applyWeights' _ _ = Nothing
            applyWeights'' :: (IsGraph a) => [[Weight]] -> Layer -> Layer -> Maybe [a -> a]
            applyWeights'' (w:ws) froml@(x:xs) tol = liftA2 (++) (applyWeights''' w x tol) (applyWeights'' ws xs tol)
            applyWeights'' [] [] _ = Just []
            applyWeights'' a b c = Nothing
            applyWeights''' :: (IsGraph a) => [Weight] -> Int -> Layer -> Maybe [a -> a]
            applyWeights''' (w:ws) from tol@(x:xs) = applyWeights''' ws from xs >>= return . (setWeight from x w :) 
            applyWeights''' e@[] f g@[] = Just []
            applyWeights''' a b c = Nothing
    learnFrom input output network = runNetwork network input >>=
                                     flip trainNetwork output
    evalError input output network = runNetwork network input >>=
                                     getOutput >>=
                                     zipWith' (-) output >>=
                                     return . (/2) . sum . map (\x -> x*x)

backpropogate :: (Show a, IsGraph a) => Layer -> Layer -> a -> NonLinearity -> NodeMap -> Maybe NodeMap
backpropogate froml tol graph nonlin errors = let
                                                vals :: Maybe [Value]
                                                vals = sequence $ map (flip getValue graph) tol
                                                links :: [[Int]]
                                                links = map (flip linksFrom graph) tol
                                                weights :: Maybe [[Weight]]
                                                weights = zipWith' findWeights tol links >>=
                                                          sequence
                                                    where
                                                        findWeights i os = sequence $ map (flip (getWeight i) graph) os
                                                errorl :: Maybe [Value]
                                                errorl = sequence $ map (flip Map.lookup errors) froml
                                              in
                                                liftA3 go weights vals errorl >>=
                                                id >>=
                                                update tol
        where
            go :: [[Weight]] -> [Value] -> [Value] -> Maybe [Value]
            go [] [] _ = Just []
            go weights@(w:ws) output@(o:os) err = let
                                                    fn = snd nonlin
                                                    nextVal = dot' w err >>= 
                                                              return . (* (fn o))
                                                    restVals = go ws os err
                                                  in
                                                    liftA2 (:) nextVal restVals
            update nodes errorl = let
                                    insertions = zipWith' Map.insert nodes errorl
                                  in
                                    fmap (foldl (flip ($)) errors) insertions

runLayer :: (Show a, IsGraph a) => Layer -> Layer -> a -> [Value] -> NonLinearity -> Maybe a
runLayer inl outl graph obias nonlin = let
                                        vals :: Maybe [Value]
                                        vals = sequence $ map (flip getValue graph) inl
                                        links :: [[Int]]
                                        links = map (flip linksFrom graph) inl
                                        weights :: Maybe [[Weight]]
                                        weights = zipWith' findWeights inl links >>=
                                                  sequence
                                            where
                                                findWeights i os = sequence $ map (flip (getWeight i) graph) os
                                       in
                                        liftA2 (go obias) (fmap transpose weights) vals >>=
                                        id >>=
                                        update outl
        where
            go :: [Value] -> [[Weight]] -> [Value] -> Maybe [Value]
            go [] [] _ = Just []
            go bias@(b:bs) weights@(w:ws) input = let
                                                    fn = fst nonlin
                                                    nextVal = (fn . (+ b)) <$> dot' input w
                                                    restVals = go bs ws input
                                                  in
                                                    liftA2 (:) nextVal restVals
            go _ _ _ = Nothing
            update nodes values = let
                                    insertions = zipWith' setValue nodes values
                                  in
                                    fmap (foldl (flip ($)) graph) insertions

{- Safe Utils -}

head' :: [a] -> Maybe a
head' (x:xs) = Just x
head' [] = Nothing

tail' :: [a] -> Maybe [a]
tail' (x:xs) = Just xs
tail' [] = Nothing

init' :: [a] -> Maybe [a]
init' [] = Nothing
init' [x] = Just []
init' (x:xs) = fmap (x:) (init' xs)

last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs

dot' :: (Num a) => [a] -> [a] -> Maybe a
dot' [] [] = Just 0
dot' (x:xs) (y:ys) = dot' xs ys >>= return . ((x*y) +)
dot' _ _ = Nothing

zipWith' :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWith' _ [] [] = Just []
zipWith' fn (x:xs) (y:ys) = zipWith' fn xs ys >>= return . (fn x y :)
zipWith' _ _ _ = Nothing

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> Maybe [d]
zipWith3' _ [] [] [] = Just []
zipWith3' fn (x:xs) (y:ys) (z:zs) = zipWith3' fn xs ys zs >>= return . (fn x y z :)
zipWith3' _ _ _ _ = Nothing

{- NonLinearities -}

sigmoid :: NonLinearity
sigmoid = (f, f')
        where
            f x = 1 / ( 1 + exp(-x) )
            f' x = x * ( 1 - x )

step :: NonLinearity
step = (f, f')
        where
            f x | x >= 0 = 1
                | x < 0 = -1
            f' 0 = 1 / 0
            f' x = 0 / 1

{- Code -}

createNetwork :: Network AdjacencyList
createNetwork = Network graph layers nonlin alpha bias
        where
            graph = Adjacency (connectGraph Map.empty) Map.empty
            layers = [[0,1],[2,3,4],[5]]
            nonlin = sigmoid
            alpha = -1.0
            bias = Map.empty
            connectGraph = Map.insert (0,2) 0
                         . Map.insert (0,3) 1
                         . Map.insert (0,4) 1
                         . Map.insert (1,2) 1
                         . Map.insert (1,3) 1
                         . Map.insert (1,4) 0
                         . Map.insert (2,5) 1
                         . Map.insert (3,5) (-2)
                         . Map.insert (4,5) 1

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

main :: IO ()
main = let
        trainingSet :: [([Value],[Value])]
        trainingSet = [([-1,-1],[1]),([-1,1],[-1]),([1,-1],[-1]),([1,1],[1])]
        trainedNetwork :: Network AdjacencyList
        trainedNetwork = trainNetwork trainingSet createNetwork
       in
        (putStrLn . show . evalErrors trainingSet) trainedNetwork >>
        (putStrLn . show) trainedNetwork >>
        (mapM_ (putStrLn . show) . mapNetwork (map fst trainingSet)) trainedNetwork
        where
            trainNetwork :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> Network a
            trainNetwork set net = fromJust $ foldl (>>=) (Just net) (replicate 10 $ flip (trainOn 10) set)
            evalErrors :: (Show a, IsGraph a) => [([Value],[Value])] -> Network a -> [Value]
            evalErrors set net = fromJust . sequence $ map (flip (uncurry evalError) net) set

