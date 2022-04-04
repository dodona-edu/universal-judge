module Encode where

import Values
import System.IO (stdout)
import Data.Int
import Data.Word

main = do
          % for statement in statements:
             sendValueH stdout (<%include file="statement.mako" args="statement=statement"/>)
             putStr "\n"
          % endfor
