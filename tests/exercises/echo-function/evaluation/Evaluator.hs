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


evaluate_value :: Map -> EvaluationResult
evaluate_value context =
    let correct = if (lookup "actual" actual) == (lookup "expected" expected) then True else False
    in evaluationResult {
        result = correct,
        readableExpected = Just (lookup "expected" expected),
        readableActual = Just (lookup "actual" actual),
        messages = [message "Hallo"]
    }
