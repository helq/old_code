{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Csv (FromRecord, decode, HasHeader(..))
import Data.Int (Int64)
import Data.Vector (Vector, length, fromList, (!))
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)

import TensorFlow.Core (TensorData, Session, Build, render, runWithFeeds, feed, unScalar, build,
                        Tensor, encodeTensorData)
import TensorFlow.Minimize (minimizeWith, adam)
import TensorFlow.Ops (placeholder, truncatedNormal, add, matMul, relu,
                      argMax, scalar, cast, oneHot, reduceMean, softmaxCrossEntropyWithLogits,
                      equal, vector)
import TensorFlow.Session (runSession)
import TensorFlow.Variable (readValue, initializedVariable, Variable)
import TensorFlow.Build (MonadBuild(..))

data IrisRecord = IrisRecord
  { field1 :: Float
  , field2 :: Float
  , field3 :: Float
  , field4 :: Float
  , label  :: Int64
  }
  deriving (Generic)

instance FromRecord IrisRecord

readIrisFromFile :: FilePath -> IO (Vector IrisRecord)
readIrisFromFile fp = do
  contents <- readFile fp
  let contentsAsBs = pack contents
  let results = decode HasHeader contentsAsBs :: Either String (Vector IrisRecord)
  case results of
    Left err -> error err
    Right records -> return records

sampleSize :: Int
sampleSize = 10

chooseRandomRecords :: Vector IrisRecord -> IO (Vector IrisRecord)
chooseRandomRecords records = do
  let numRecords = Data.Vector.length records
  chosenIndices <- take sampleSize <$> shuffleM [0..(numRecords - 1)]
  return $ fromList $ map (records !) chosenIndices

irisFeatures :: Int64
irisFeatures = 4

irisLabels :: Int64
irisLabels = 3

convertRecordsToTensorData :: Vector IrisRecord -> (TensorData Float, TensorData Int64)
convertRecordsToTensorData records = (input, output)
  where
    numRecords = Data.Vector.length records
    input = encodeTensorData [fromIntegral numRecords, irisFeatures]
      (fromList $ concatMap recordToInputs records)
    output = encodeTensorData [fromIntegral numRecords] (label <$> records)
    recordToInputs :: IrisRecord -> [Float]
    recordToInputs rec = [field1 rec, field2 rec, field3 rec, field4 rec]

data Model = Model
  { train :: TensorData Float -- Training input
          -> TensorData Int64 -- Training output
          -> Session ()
  , errorRate :: TensorData Float -- Test input
              -> TensorData Int64 -- Test output
              -> Session Float
  }

buildNNLayer :: MonadBuild m
             => Int64 -> Int64 -> Tensor v Float
             -> m (Variable Float, Variable Float, Tensor Build Float)
buildNNLayer inputSize outputSize input = do
  weights <- truncatedNormal (vector [inputSize, outputSize]) >>= initializedVariable
  bias <- truncatedNormal (vector [outputSize]) >>= initializedVariable
  let results = (input `matMul` readValue weights) + readValue bias
  return (weights, bias, results)

createModel :: MonadBuild m => m Model
createModel = do
  let batchSize = -1 -- Allows variable sized batches
  let numHiddenUnits = 10
  inputs <- placeholder [batchSize, irisFeatures]
  outputs <- placeholder [batchSize]

  (hiddenWeights, hiddenBiases, hiddenResults) <-
    buildNNLayer irisFeatures numHiddenUnits inputs
  let rectifiedHiddenResults = relu hiddenResults
  (finalWeights, finalBiases, finalResults) <-
    buildNNLayer numHiddenUnits irisLabels rectifiedHiddenResults

  actualOutput <- render $ cast $ argMax finalResults (scalar (1 :: Int64))
  let correctPredictions = equal actualOutput outputs
  errorRate_ <- render $ 1 - reduceMean (cast correctPredictions)

  let outputVectors = oneHot outputs (fromIntegral irisLabels) 1 0
  let loss = reduceMean $ fst $ softmaxCrossEntropyWithLogits finalResults outputVectors
  let params = [hiddenWeights, hiddenBiases, finalWeights, finalBiases]
  train_ <- minimizeWith adam loss params

  return Model
    { train = \inputFeed outputFeed ->
        runWithFeeds
          [ feed inputs inputFeed
          , feed outputs outputFeed
          ]
          train_
    , errorRate = \inputFeed outputFeed -> unScalar <$>
        runWithFeeds
          [ feed inputs inputFeed
          , feed outputs outputFeed
          ]
          errorRate_
    }

runIris :: FilePath -> FilePath -> IO ()
runIris trainingFile testingFile = runSession $ do
  -- Preparation
  trainingRecords <- liftIO $ readIrisFromFile trainingFile
  testRecords <- liftIO $ readIrisFromFile testingFile
  model <- build createModel

  -- Training
  forM_ ([0..1000] :: [Int]) $ \i -> do
    trainingSample <- liftIO $ chooseRandomRecords trainingRecords
    let (trainingInputs, trainingOutputs) = convertRecordsToTensorData trainingSample
    train model trainingInputs trainingOutputs
    when (i `mod` 100 == 0) $ do
      err <- errorRate model trainingInputs trainingOutputs
      liftIO $ putStrLn $ "Current training error " ++ show (err * 100)

  liftIO $ putStrLn ""

  -- Testing
  let (testingInputs, testingOutputs) = convertRecordsToTensorData testRecords
  testingError <- errorRate model testingInputs testingOutputs
  liftIO $ putStrLn $ "test error " ++ show (testingError * 100)

  return ()

main :: IO ()
main = runIris "data/iris-training-modified.data" "data/iris-test-modified.data"
