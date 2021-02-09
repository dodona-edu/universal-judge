{-# LANGUAGE NamedFieldPuns #-}
## Code to execute one context.
<%! from tested.languages.generator import _TestcaseArguments %>
<%! from tested.serialisation import Statement, Expression, Assignment %>
<%! from tested.utils import get_args %>
module ${execution_name} where

##################################
## Setup                        ##
##################################

import System.IO (hPutStr, stderr, stdout, hFlush)
import System.Environment
import qualified Values
import Control.Monad.Trans.Class
import Control.Exception
import EvaluationUtils

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    import qualified ${name}
% endfor

## Import the solution.
import qualified ${submission_name}


value_file = "${value_file}"
exception_file = "${exception_file}"

## Write the separator and flush to ensure the output is in the files.
writeSeparator :: IO ()
writeSeparator = do
    hPutStr stderr "--${secret_id}-- SEP"
    hPutStr stdout "--${secret_id}-- SEP"
    appendFile value_file "--${secret_id}-- SEP"
    appendFile exception_file "--${secret_id}-- SEP"
    hFlush stdout
    hFlush stderr


writeContextSeparator :: IO ()
writeContextSeparator = do
    hPutStr stderr "--${context_secret_id}-- SEP"
    hPutStr stdout "--${context_secret_id}-- SEP"
    appendFile value_file "--${context_secret_id}-- SEP"
    appendFile exception_file "--${context_secret_id}-- SEP"
    hFlush stdout
    hFlush stderr


##################################
## Predefined functions         ##
##################################

## Send a value to TESTed
sendValue :: Values.Typeable a => a -> IO ()
sendValue = Values.sendValue value_file

## Send an exception to TESTed
sendException :: Exception e => Maybe e -> IO ()
sendException = Values.sendException exception_file

## Send the result of a language specific value evaluator to TESTed.
sendSpecificValue :: EvaluationResult -> IO ()
sendSpecificValue = Values.sendEvaluated value_file

## Send the result of a language specific exception evaluator to TESTed.
sendSpecificException :: EvaluationResult -> IO ()
sendSpecificException = Values.sendEvaluated exception_file


## Exception handler
handleException :: Exception e => (Either e a) -> Maybe e
handleException (Left e) = Just e
handleException (Right _) = Nothing


% for i, ctx in enumerate(contexts):
    ${execution_name.lower()}Context${i} :: IO ()
    ${execution_name.lower()}Context${i} = do
        ${ctx.before}
        % for testcase in ctx.testcases:
            writeSeparator

            ## In Haskell we do not actually have statements, so we need to keep them separate.
            ## Additionally, exceptions with "statements" are not supported at this time.
            % if isinstance(testcase.command, get_args(Assignment)):
                <%include file="statement.mako" args="statement=testcase.command,root=False" />
            % else:
                result${loop.index} <- catch
                    (<%include file="statement.mako" args="statement=testcase.command,lifting=True" />
                        >>= \r -> <%include file="statement.mako" args="statement=testcase.input_statement('r')" />
                        >> let ee = (Nothing :: Maybe SomeException) in <%include file="statement.mako" args="statement=testcase.exception_statement('ee')"/>)
                    (\e -> let ee = (Just (e :: SomeException)) in <%include file="statement.mako" args="statement=testcase.exception_statement('ee')"/>)
            % endif

        % endfor
        ${ctx.after}
        putStr ""
% endfor

## Main function of the context.
main :: IO ()
main = do
    writeContextSeparator

    % if run_testcase.exists:
        let mainArgs = [\
            % for argument in run_testcase.arguments:
                <%include file="value.mako" args="value=argument"/>\
            % endfor
        ]
        result <- try (withArgs mainArgs ${submission_name}.main) :: IO (Either SomeException ())
        let ee = handleException result in <%include file="statement.mako" args="statement=run_testcase.exception_statement('ee')"/>
    % endif

    % for i, ctx in enumerate(contexts):
        writeContextSeparator
        ${execution_name.lower()}Context${i}
    % endfor

    putStr ""
