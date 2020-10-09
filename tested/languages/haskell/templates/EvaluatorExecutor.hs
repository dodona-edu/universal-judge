<%page args="evaluator,function" />
module EvaluatorExecutor where

import qualified ${evaluator}
import Values
import System.IO (stdout)


main = do x <- return $ <%include file="function.mako" args="function=function" />
          sendEvaluatedH stdout x
