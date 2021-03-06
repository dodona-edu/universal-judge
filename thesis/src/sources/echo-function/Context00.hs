{-# LANGUAGE NamedFieldPuns #-}
module Context00 where
import System.IO (hPutStr, stderr, stdout, hFlush)
import System.Environment
import qualified Values
import Control.Monad.Trans.Class
import Control.Exception
import EvaluationUtils
import qualified Submission
value_file = "yq5AlxCOr_values.txt"
exception_file = "yq5AlxCOr_exceptions.txt"
writeSeparator :: IO ()
writeSeparator = do
    hPutStr stderr "--yq5AlxCOr-- SEP"
    hPutStr stdout "--yq5AlxCOr-- SEP"
    appendFile value_file "--yq5AlxCOr-- SEP"
    appendFile exception_file "--yq5AlxCOr-- SEP"
    hFlush stdout
    hFlush stderr
sendValue :: Values.Typeable a => a -> IO ()
sendValue = Values.sendValue value_file
sendException :: Exception e => Maybe e -> IO ()
sendException = Values.sendException exception_file
sendSpecificValue :: EvaluationResult -> IO ()
sendSpecificValue = Values.sendEvaluated value_file
sendSpecificException :: EvaluationResult -> IO ()
sendSpecificException = Values.sendEvaluated exception_file
handleException :: Exception e => (Either e a) -> Maybe e
handleException (Left e) = Just e
handleException (Right _) = Nothing
main = do
    writeSeparator
    writeSeparator
    result0 <- catch
        (return (Submission.echo ("input-1"))
            >>= \r -> sendValue (r)
            >> let ee = (Nothing :: Maybe SomeException) in sendException (ee))
        (\e -> let ee = (Just (e :: SomeException)) in sendException (ee))
    writeSeparator
    result1 <- catch
        (return (Submission.echo ("input-2"))
            >>= \r -> sendValue (r)
            >> let ee = (Nothing :: Maybe SomeException) in sendException (ee))
        (\e -> let ee = (Just (e :: SomeException)) in sendException (ee))
    putStr ""