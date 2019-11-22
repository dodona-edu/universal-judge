module Evaluator${context_id} where

import Values

##
## def value_write(value):
##     value_file.write(value)


evaluated :: Bool -> Text -> Text -> [Text] -> IO ()
evaluated result expected actual messages = sendSpecificEvaluated "${output_file}"


send :: Typeable a => a -> IO ()
send = sendValue "${output_file}"


% for additional in additionals:
    ${additional.value_code}

% endfor
