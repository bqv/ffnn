{-# LANGUAGE FlexibleInstances #-}

module FFNN (
    module FFNN,
    module Control.Monad.Writer,
    module Control.Monad.Trans.Except
) where

import Util
import Data.List
import Data.Maybe
import Data.Either
import Control.Lens
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map

{- Types -}

type Weight = Double
type Value = Double

{- Graphs -}

type NodeMap = Map.Map Int Value

data AdjacencyList = Adjacency (Map.Map (Int, Int) Weight) NodeMap deriving (Show)
data Matrix = Matrix [[Maybe Weight]] NodeMap deriving (Show)

class IsGraph a where
    getWeight :: Int -> Int -> a -> Maybe Weight
    getValue :: Int -> a -> Maybe Value
    setWeight :: Int -> Int -> Weight -> a -> a
    setValue :: Int -> Value -> a -> a
    linksFrom :: Int -> a -> [Int]
    listNodes :: a -> [Int]

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
    listNodes (Adjacency graph _) = nub . sort . foldl (\l (a,b) -> a:b:l) [] . Map.keys $ graph

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
    listNodes (Matrix graph _) = map fst . zip [0..] $ graph 

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
    getOutput :: a -> ExceptT String (Writer [String]) [Value]
    runNetwork :: a -> [Value] -> ExceptT String (Writer [String]) a
    trainNetwork :: a -> [Value] -> ExceptT String (Writer [String]) a
    learnFrom :: [Value] -> [Value] -> a -> ExceptT String (Writer [String]) a
    evalError :: [Value] -> [Value] -> a -> ExceptT String (Writer [String]) Value

instance (Show a, IsGraph a) => Neural (Network a) where
    getOutput network = maybe (throwE "No layers in graph") pure (last' $ getLayers network) >>=
                        maybe (throwE "Missing value in output") pure . sequence . map (flip getValue (getGraph network)) >>=
                        traceLogF (("Getting output: "++) . show)
    runNetwork network input = let
                                layers :: Layers
                                layers = getLayers network
                                biases :: Maybe [Double]
                                biases = head' layers >>=
                                         return . map (flip (Map.findWithDefault 0) $ getBias network)
                                biasedInputs :: Maybe [Double]
                                biasedInputs = biases >>= zipWith' ($) (map (+) input)
                                insertions :: (Show a, IsGraph a) => Either String [a -> a]
                                insertions = maybe (Left "Error inserting inputs") Right $ (zipWith' setValue) <$> (head' layers) <*> biasedInputs >>= id
                                inputGraph = flip (foldl (.) id) (getGraph network) <$> insertions
                               in
                                ExceptT (return inputGraph) >>=
                                go layers >>=
                                return . updateGraph network
        where
            {- Flow layer x to y -}
            go :: (Show a, IsGraph a) => Layers -> a -> ExceptT String (Writer [String]) a
            go (x:y:ys) g = let
                                biases :: [Double]
                                biases = map (flip (Map.findWithDefault 0) $ getBias network) y
                            in
                                runLayer x y g biases (getNonlin network) >>=
                                go (y:ys)
            go _ g = return g
    trainNetwork network predic = let
                                    reverseLayers :: Layers
                                    reverseLayers = reverse $ getLayers network
                                    output :: Maybe [Double]
                                    output = head' reverseLayers >>=
                                             sequence . map (flip getValue (getGraph network))
                                    errorl :: Maybe [Double]
                                    errorl = output >>= zipWith' findErr predic
                                                where
                                                    findErr :: Double -> Double -> Double
                                                    findErr ex out = (ex - out) * (($ out) . snd $ getNonlin network)
                                    insertions :: ExceptT String (Writer [String]) [NodeMap -> NodeMap]
                                    insertions = maybe (throwE "Couldn't calculate output errors") pure $ fmap (zipWith' Map.insert) (head' reverseLayers) <*> errorl >>= id
                                    errMap :: ExceptT String (Writer [String]) NodeMap
                                    errMap = flip (foldl (.) id) Map.empty <$> insertions >>= calcErr reverseLayers
                                    errList :: ExceptT String (Writer [String]) [[Maybe Double]]
                                    errList = fmap (flip map reverseLayers . map . flip Map.lookup) errMap
                                  in
                                    writeLog ("Training network: "++(show network)) >>
                                    (sequence . map sequence) <$> errList >>=
                                    maybe (throwE "Couldn't calculate error values") pure . liftA2 (mapErr $ getGraph network) (tail' reverseLayers) >>=
                                    maybe (throwE "Failed to map error values") pure . fmap reverse >>=
                                    traceLogF (("Got Errors: "++) . show) >>=
                                    applyWeights >>=
                                    traceLogF (("Trained network: "++) . show) . updateGraph network
        where
            calcErr :: Layers -> NodeMap -> ExceptT String (Writer [String]) NodeMap
            calcErr (x:y:ys) errMap = backpropogate x y (getGraph network) (getNonlin network) errMap >>=
                                      calcErr (y:ys)
            calcErr _ m = lift $ pure m
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
            applyWeights' :: (IsGraph a) => [[[Weight]]] -> Layers -> ExceptT String (Writer [String]) [a -> a]
            applyWeights' (w:ws) (x:y:ys) = liftA2 (++) (applyWeights'' w x y) (applyWeights' ws (y:ys))
            applyWeights' [] [x] = pure []
            applyWeights' _ _ = throwE "Error applying weights"
            applyWeights'' :: (IsGraph a) => [[Weight]] -> Layer -> Layer -> ExceptT String (Writer [String]) [a -> a]
            applyWeights'' (w:ws) froml@(x:xs) tol = liftA2 (++) (applyWeights''' w x tol) (applyWeights'' ws xs tol)
            applyWeights'' [] [] _ = pure []
            applyWeights'' a b c = throwE "Error applying weights"
            applyWeights''' :: (IsGraph a) => [Weight] -> Int -> Layer -> ExceptT String (Writer [String]) [a -> a]
            applyWeights''' (w:ws) from tol@(x:xs) = applyWeights''' ws from xs >>= return . (setWeight from x w :) 
            applyWeights''' e@[] f g@[] = pure []
            applyWeights''' a b c = throwE "Error applying weights"
    learnFrom input output network = runNetwork network input >>=
                                     flip trainNetwork output
    evalError input output network = runNetwork network input >>=
                                     getOutput >>=
                                     maybe (throwE "Couldn't match output with supplied data") pure . zipWith' (-) output >>=
                                     return . (/2) . sum . map (\x -> x*x)

backpropogate :: (Show a, IsGraph a) => Layer -> Layer -> a -> NonLinearity -> NodeMap -> ExceptT String (Writer [String]) NodeMap
backpropogate froml tol graph nonlin errors = let
                                                vals :: Either String [Value]
                                                vals = maybe (Left "Missing value in backpropogation") Right . sequence $ map (flip getValue graph) tol
                                                links :: [[Int]]
                                                links = map (flip linksFrom graph) tol
                                                weights :: Either String [[Weight]]
                                                weights = maybe (Left "Mismatched graph during backpropogation") Right $ zipWith' findWeights tol links >>= sequence
                                                    where
                                                        findWeights :: Int -> [Int] -> Maybe [Weight]
                                                        findWeights i os = sequence $ map (flip (getWeight i) graph) os
                                                errorl :: Either String [Value]
                                                errorl = maybe (Left "Missing error in backpropogation") Right . sequence $ map (flip Map.lookup errors) froml
                                              in
                                                writeLog ("Backpropogating from layer: "++(show froml)) >>
                                                ExceptT (return $ liftA3 go weights vals errorl) >>=
                                                maybe (throwE "Failed to calculate backpropogation values") pure >>=
                                                maybe (throwE "Mismatched layer in backpropogation") pure . update tol
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

runLayer :: (Show a, IsGraph a) => Layer -> Layer -> a -> [Value] -> NonLinearity -> ExceptT String (Writer [String]) a
runLayer inl outl graph obias nonlin = let
                                        vals :: Either String [Value]
                                        vals = maybe (Left "Missing value in feedforward") Right . sequence $ map (flip getValue graph) inl
                                        links :: [[Int]]
                                        links = map (flip linksFrom graph) inl
                                        weights :: Either String [[Weight]]
                                        weights = maybe (Left "Mismatched graph during feedforward") Right $ zipWith' findWeights inl links >>= sequence
                                            where
                                                findWeights :: Int -> [Int] -> Maybe [Weight]
                                                findWeights i os = sequence $ map (flip (getWeight i) graph) os
                                       in
                                        writeLog ("Feedforwarding from layer: "++(show inl)) >>
                                        ExceptT (return $ liftA2 (go obias) (fmap transpose weights) vals) >>=
                                        maybe (throwE "Failed to calculated feedforward values") pure >>=
                                        maybe (throwE "Mismatched layer in feedforward") pure . update outl
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

trainOn :: (Show a, IsGraph a) => Int -> Network a -> [([Value],[Value])] -> ExceptT String (Writer [String]) (Network a)
trainOn n network dataset = foldl (>>=) (lift $ pure network) $ map (trainN n) dataset
        where
            trainN :: (Show a, IsGraph a) => Int -> ([Value],[Value]) -> Network a -> ExceptT String (Writer [String]) (Network a)
            trainN 0 input net = lift $ pure net
            trainN n input net = uncurry learnFrom input net >>=
                                 trainN (n-1) input

mapNetwork :: (Show a, IsGraph a) => [[Value]] -> Network a -> ExceptT String (Writer [String]) [([Value],[Value])]
mapNetwork inputs net = let
                            outputs :: ExceptT String (Writer [String]) [[Value]]
                            outputs = sequence $ map ((>>= getOutput) . runNetwork net) inputs
                        in
                            zip inputs <$> outputs

createNetwork' :: Network AdjacencyList
createNetwork' = Network graph layers nonlin alpha bias
        where
            graph = Adjacency (connectGraph Map.empty) Map.empty
            layers = [[0,1],[2,3,4],[5]]
            nonlin = step
            alpha = -0.01
            bias = Map.empty
            connectGraph = Map.insert (0,2) (0)
                         . Map.insert (0,3) (1)
                         . Map.insert (0,4) (1)
                         . Map.insert (1,2) (1)
                         . Map.insert (1,3) (1)
                         . Map.insert (1,4) (0)
                         . Map.insert (2,5) (1)
                         . Map.insert (3,5) (-2)
                         . Map.insert (4,5) (1)

createNetwork :: Network AdjacencyList
createNetwork = Network graph layers nonlin alpha bias
        where
            graph = Adjacency (connectGraph Map.empty) Map.empty
            layers = [[0,1],[2,3],[4]]
            nonlin = sigmoid
            alpha = -0.004
            bias = createNodeMap Map.empty
            connectGraph = Map.insert (0,2) (-11.62)
                         . Map.insert (0,3) (12.88)
                         . Map.insert (1,2) (10.99)
                         . Map.insert (1,3) (-13.13)
                         . Map.insert (2,4) (13.34)
                         . Map.insert (3,4) (13.13)
            createNodeMap = Map.insert 0 (0)
                          . Map.insert 1 (0)
                          . Map.insert 2 (-6.06)
                          . Map.insert 3 (-7.19)
                          . Map.insert 4 (-6.56)

