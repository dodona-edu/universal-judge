import System.IO (writeFile)

echoFunction :: FilePath -> String -> IO ()
echoFunction filename content = do
  let contentWithNewline = content ++ "\n"
  writeFile filename contentWithNewline
  return ()
