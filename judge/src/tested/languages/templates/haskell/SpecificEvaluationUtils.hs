{-# LANGUAGE OverloadedStrings #-}
module SpecificEvaluationUtils where

import Values

-- | Report the result of an evaluation to the judge. This method should only
-- be called once, otherwise things will break.
-- Note that the first parameter, the file path, will be provided and must not
-- be changed. It should be passed to `evaluated` as is.
evaluated :: FilePath -- ^ Where the result should be written.
          -> Bool     -- ^ The result of the evaluation.
          -> String   -- ^ A string version of the actual value. Optional.
          -> String   -- ^ A string version of the expected value. Optional.
          -> [String] -- ^ A list of messages for the user.
          -> IO ()
evaluated = sendSpecificEvaluated
