{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where

import EvaluationUtils
import Control.Exception

evaluate :: Maybe (SomeException) -> EvaluationResult
evaluate Nothing = evaluationResult
evaluate (Just x) =
    case fromException x of
        Just (x :: ArithException) -> handleA x
        nothing -> evaluationResult


handleA :: ArithException -> EvaluationResult
handleA DivideByZero = evaluationResult {
                          result = True,
                          readableExpected = Just $ show DivideByZero,
                          readableActual = Just $ show DivideByZero
                      }
handleA _ = evaluationResult