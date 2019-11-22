{-# OverloadedStrings #-}
module EvaluatorCustom where

import Values

evaluated :: Bool -> [Text] -> IO ()
evaluated = sendCustomEvaluated "${output_file}"


${evaluator_code}

main = evaluate \
<%include file="value.mako" args="value=expected" />
 \
<%include file="value.mako" args="value=actual" />
