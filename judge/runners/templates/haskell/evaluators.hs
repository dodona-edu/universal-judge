{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Evaluator${context_id} where

import Values
import Control.Monad.Trans.Class

##
## def value_write(value):
##     value_file.write(value)


evaluated :: Bool -> String -> String -> [String] -> IO ()
evaluated = sendSpecificEvaluated "${output_file}"


send :: Typeable a => a -> IO ()
send = sendValue "${output_file}"


% for additional in additionals:
    ${additional.value_code}

% endfor
