{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverloadedLists #-}

-- Code from book TensorFlow guide from Monday Morning Haskell

module Main where

--import Lib

import           Control.Monad (replicateM_)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)

import           TensorFlow.Core (Tensor, Value, feed, encodeTensorData, Scalar(..), expr)
import           TensorFlow.Ops (placeholder, reduceSum, matMul)
import           TensorFlow.GenOps.Core (square)
import           TensorFlow.Variable (readValue, initializedVariable, Variable)
import           TensorFlow.Session (runSession, run, runWithFeeds)
import           TensorFlow.Minimize (gradientDescent, minimizeWith)
import           TensorFlow.Types (Shape(..))

-- We'll take two vectors (of equal size representing the inputs and expected
-- outputs of a linear equation.
basicExample :: Vector Float -> Vector Float -> IO (Float, Float)
basicExample xInput yInput = runSession $ do
  -- Everything below this   ^^ takes place in the "Session" monad, which we
  -- run with "runSession".

  -- Get the sizes of out input and expected output
  let xSize = fromIntegral $ Vector.length xInput
  let ySize = fromIntegral $ Vector.length yInput

  -- Make a "weight" variable (slope of our line) with initial value 3
  (w :: Variable Float) <- initializedVariable 3

  -- Make a "bias" variable (y-intercept of line) with initial value 1
  (b :: Variable Float) <- initializedVariable 1

  -- Create "placeholders" with the size of our input and output
  (x :: Tensor Value Float) <- placeholder $ Shape [xSize]
  (y :: Tensor Value Float) <- placeholder $ Shape [ySize]

  -- Make our "model", which multiplies the weights by the input and then adds
  -- the bias. Notice we use "readValue" to use operations on variables. This
  -- is our "actual output" value.
  let linear_model = (readValue w `matMul` x) + readValue b

  -- Find the difference between actual output and expected output y, and then
  -- square it.
  let square_deltas = square (linear_model - expr y)

  -- Get our "loss" function by taking the reduced sum of the above, then
  -- "train" our model by using the gradient descent optimizer.  Notice we pass
  -- our weights and bias as the parameters that change.
  let loss = reduceSum square_deltas
  trainStep <- minimizeWith (gradientDescent 0.01) loss [w,b]

  -- "Train" our model, but passing our input and output values as "feeds" to
  -- fill in the placeholder values.
  let trainWithFeeds xF yF = runWithFeeds [feed x xF, feed y yF] trainStep

  -- Run this training step 1000 times. Encode our input as "TensorData"
  replicateM_ 1000 $
    trainWithFeeds
      (encodeTensorData (Shape [xSize]) xInput)
      (encodeTensorData (Shape [ySize]) yInput)

  -- "Run" our variables to see their learned values and return them
  (Scalar w_learned, Scalar b_learned) <- run (readValue w, readValue b)

  return (w_learned, b_learned)

-- Usage of the "basicExample" function. Should result in a tuple like: (5, -1)
main :: IO ()
main = do
  results <- basicExample
    (Vector.fromList [1.0, 2.0, 3.0, 4.0])
    (Vector.fromList [4.0, 9.0, 14.0, 19.0])
--    (Vector [1.0, 2.0, 3.0, 4.0]) -- <- this doesn't work because Data.Vector does NOT export Vector data constructor
--    (Vector [4.0, 9.0, 14.0, 19.0])

  print results
