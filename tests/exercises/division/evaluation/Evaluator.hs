{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where

import EvaluationUtils
import Control.Exception

evaluate :: Maybe (SomeException) -> EvaluationResult
evaluate Nothing = evaluationResult {
                      readableExpected = Just $ show DivideByZero,
                      readableActual = Just "",
                      messages = [message "Expected DivideByZero, got nothing."]
                  }
evaluate (Just x) =
    case fromException x of
        Just (x :: ArithException) -> handleA x
        nothing -> evaluationResult {
                       readableExpected = Just $ show DivideByZero,
                       readableActual = Just "",
                       messages = [message "Expected DivideByZero, got nothing."]
                   }


runtime :: Maybe (SomeException) -> EvaluationResult
runtime _ = evaluationResult {
                      readableExpected = Just $ show DivideByZero,
                      readableActual = Just $ show (100 `div` 0),
                      messages = [message "Expected DivideByZero, got nothing."]
                  }


handleA :: ArithException -> EvaluationResult
handleA DivideByZero = evaluationResult {
                          result = True,
                          readableExpected = Just $ show DivideByZero,
                          readableActual = Just $ show DivideByZero
                      }
handleA other = evaluationResult {
                readableExpected = Just $ show DivideByZero,
                readableActual = Just $ show other,
                messages = [message "Expected DivideByZero, got something else."]
            }
