module Encode where

import Values
import System.IO (stdout)


main = do 
          % for statement in statements:
             sendValueH stdout (<%include file="statement.mako" args="statement=statement"/>)
             putStr "\n"
          % endfor  
