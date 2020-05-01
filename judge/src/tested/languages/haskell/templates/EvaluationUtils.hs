{-# LANGUAGE OverloadedStrings #-}
module EvaluationUtils where

data Message = Message {
    description :: String,
    format :: String,
    permission :: Maybe String
}

data EvaluationResult = EvaluationResult {
    result :: Bool,
    readableExpected :: Maybe (String),
    readableActual :: Maybe (String),
    messages :: [Message]
}

message description = Message {
    description = description,
    format = "text",
    permission = Nothing
}

evaluationResult = EvaluationResult {
    result = False,
    readableExpected = Nothing,
    readableActual = Nothing,
    messages = []
}
