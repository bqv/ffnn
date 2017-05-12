
import Data.List
import Control.Monad.State
import Debug.Trace

type Weight = Double
type Value = Double

data Network = Net [Int] [[Value]] (Value -> Value)

nodes :: [Int]
nodes = [2,3,1]

bias :: [[Value]]
bias = [[0,0],[0,0,0],[0]]

alpha :: Value
alpha = 0.004

initial_weights :: [[[Weight]]]
initial_weights = [[[0,1],[1,1],[1,0]],[[1,-2,1]]]

sigmoid :: Double -> Double
sigmoid x = 1 / ( 1 + exp(-x) )

sigmoid' :: Double -> Double
sigmoid' x = x * ( 1 - x )

step :: Double -> Double
step x | x >= 0 = 1
       | x < 0 = -1

dot :: (Num a) => [a] -> [a] -> a
dot [] [] = 0
dot (x:xs) (y:ys) = (x*y) + dot xs ys
dot _ _ = error "Mismatched dimensions in dot product"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' fn (x:xs) (y:ys) = fn x y : zipWith' fn xs ys
zipWith' _ _ _ = error "Mismatched dimensions in zip"

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' _ [] [] [] = []
zipWith3' fn (x:xs) (y:ys) (z:zs) = fn x y z : zipWith3' fn xs ys zs
zipWith3' _ _ _ _ = error "Mismatched dimensions in zip3"

run :: Network -> [[[Weight]]] -> [Value] -> [[Value]]
run (Net nodes bias fn) weights input = input : runNetwork (tail nodes) (tail bias) weights fn input

runNetwork :: [Int] -> [[Value]] -> [[[Weight]]] -> (Value -> Value) -> [Value] -> [[Value]]
runNetwork [] _ _ _ _ = []
runNetwork nodes@(x:xs) bias@(y:ys) weights@(z:zs) fn input = let
                                                                layer = runLayer x y z fn input
                                                              in
                                                                layer : runNetwork xs ys zs fn layer

runLayer :: Int -> [Value] -> [[Weight]] -> (Value -> Value) -> [Value] -> [Value]
runLayer 0 values _ _ _ = values
runLayer n bias weights fn input = update (head bias) : runLayer (n-1) (tail bias) (tail weights) fn input
        where
            update :: Value -> Value
            update x = fn $ dot input (head weights) + x

calcError :: Network -> [[[Weight]]] -> ([Value],[Value]) -> Value
calcError net weights (inp, out) = (/2) . sum . map (\x -> x*x) . zipWith' (-) out . last $ run net weights inp

trainM :: Network -> ([Value],[Value]) -> State [[[Weight]]] ()
trainM net io = get >>= put . flip (train net) io

train :: Network -> [[[Weight]]] -> ([Value],[Value]) -> [[[Weight]]]
train net@(Net nodes bias fn) weights io@(inp, out) = let
                                                        state = run net weights inp
                                                        error = calcError net weights io
                                                      in
                                                        traceShow ("error:", calcError net weights io) $ backpropogate nodes weights state out error

backpropogate :: [Int] -> [[[Weight]]] -> [[Value]] -> [Value] -> Value -> [[[Weight]]]
backpropogate nodes weights state out err = mapErrors . reverse $ calcErr False (reverse weights) (reverse state) [err]
        where
            mapErrors :: [[Value]] -> [[[Weight]]]
            mapErrors errors = zipWith3' (\error weight input -> map (zipWith3' (\e i w -> w+(alpha*e*i)) error input) weight) (init errors) weights (init state)
            calcErr :: Bool -> [[[Weight]]] -> [[Value]] -> [Value] -> [[Value]]
            calcErr _ _ [] _ = []
            calcErr False rweights rstate errors = let
                                                    nexterr expected output = (expected - output) * sigmoid'(output)
                                                    layer = zipWith' nexterr out (head rstate)
                                                   in
                                                    layer : calcErr True rweights (tail rstate) layer
            calcErr True rweights rstate errors = let
                                                    tweights = transpose (head rweights)
                                                    deltas = map (dot errors) tweights
                                                    layer = zipWith' (*) deltas (head rstate)
                                                  in
                                                    layer : calcErr True (tail rweights) (tail rstate) layer

iterateM :: (Monad m) => m () -> [m ()]
iterateM monad = go monad
        where
            go m = m : go (m >> monad)

main :: IO ()    
main = do        
    let net = Net nodes bias sigmoid
    putStrLn . show $ map (head . last . run net initial_weights) [[-1,-1],[-1,1],[1,-1],[1,1]]
    putStrLn . show $ map (calcError net initial_weights) [([-1,-1],[1]),([-1,1],[-1]),([1,-1],[-1]),([1,1],[1])]
    putStrLn . show $ map (train net initial_weights) [([-1,-1],[1]),([-1,1],[-1]),([1,-1],[-1]),([1,1],[1])]
    let weights = flip execState initial_weights . (!! 100) . iterateM $ mapM_ ((!! 10) . iterateM . trainM net) [([-1,-1],[1]),([-1,1],[-1]),([1,-1],[-1]),([1,1],[1])]
    putStrLn . show $ map (head . last . run net weights) [[-1,-1],[-1,1],[1,-1],[1,1]]
