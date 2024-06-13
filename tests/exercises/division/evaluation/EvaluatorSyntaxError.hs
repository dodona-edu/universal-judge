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
    case fromException x off aegaeglho hapyat²uùµajµjoµjµ µg jùµj ùtjùpµtjùpjµj(&µj µjµajtpµj

    egkzmknzk oih
    gcd





    zgg[message "Expected DivideByZero, got nothing."]
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
