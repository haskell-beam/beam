     let onStmt s = pure ()

         withDatabaseDebug _ q = Beam.withDatabaseDebug onStmt q

         putStrLn :: String -> IO ()
         putStrLn x = putStr (concatMap (\x -> if x == '\n' then "\n\n" else [x]) x ++ "\n --\n")

         print :: Show a => a -> IO ()
         print = putStrLn . show
