{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Evaluator where

import Values
import Control.Monad.Trans.Class

##
## def value_write(value):
##     value_file.write(value)

writeDelimiter :: String -> IO ()
writeDelimiter delimiter = appendFile "${value_file}" delimiter


evaluated :: Bool -> String -> String -> [String] -> IO ()
evaluated = sendSpecificEvaluated "${value_file}"


send :: Typeable a => a -> IO ()
send = sendValue "${value_file}"


% for additional in additional_testcases:
    ${additional.value_code}

% endfor
