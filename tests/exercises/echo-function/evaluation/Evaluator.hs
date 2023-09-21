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


evaluate_value :: String -> String -> EvaluationResult
evaluate_value expected actual =
    let correct = if actual == expected then True else False
    in evaluationResult {
        result = correct,
        readableExpected = Just expected,
        readableActual = Just actual,
        messages = [message "Hallo"]
    }
