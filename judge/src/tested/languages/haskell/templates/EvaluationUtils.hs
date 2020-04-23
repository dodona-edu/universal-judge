{-# LANGUAGE OverloadedStrings #-}
module EvaluationUtils where

data EvaluationResult = EvaluationResult {
    result :: Bool,
    readableExpected :: Maybe (String),
    readableActual :: Maybe (String),
    messages :: [String]
}

evaluationResult = EvaluationResult {
    result = False,
    readableExpected = Nothing,
    readableActual = Nothing,
    messages = []
}
