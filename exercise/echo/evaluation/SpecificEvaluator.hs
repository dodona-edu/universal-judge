module SpecificEvaluator where

import SpecificEvaluationUtils as U

evaluate :: FilePath -> String -> IO ()
evaluate f value = U.evaluated f (value == "test-25") "expected" (show value) ["Hallo specific!"]
