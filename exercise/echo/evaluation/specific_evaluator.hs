
evaluate :: String -> IO ()
evaluate value = evaluated (value == "test-25") "expected" (show value) ["Hallo specific!"]
