{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where

import EvaluationUtils
import Control.Exception

evaluate :: String -> EvaluationResult
evaluate value  =
    let correct = if value == "correct" then True else False
    in evaluationResult {
        result = correct,
        readableExpected = Just "correct",
        readableActual = Just value,
        messages = [message "Hallo"]
    }

evaluateSum :: String -> Integer -> EvaluationResult
evaluateSum value the_sum =
    let correct = the_sum == 10
    in evaluationResult {
        result = correct,
        readableExpected = Just "correct",
        readableActual = Just value,
        messages = [message "Hallo"]
    }
