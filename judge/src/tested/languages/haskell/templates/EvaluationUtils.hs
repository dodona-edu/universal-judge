{-# OverloadedStrings #-}
<%page args="evaluator,expected,actual,arguments" />
module EvaluationUtils where

import Values

-- | Report the result of an evaluation to the judge. This method should only
-- be called once, otherwise things will break.
-- Note that the first parameter, the file path, will be provided and must not
-- be changed. It should be passed to `evaluated` as is.
evaluated :: Bool       -- ^ The result of the evaluation.
          -> Maybe Text -- ^ A string version of the actual value. Optional.
          -> Maybe Text -- ^ A string version of the expected value. Optional.
          -> [Text]     -- ^ A list of messages for the user.
          -> IO ()
evaluated = sendCustomEvaluated
