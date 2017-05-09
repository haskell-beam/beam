     stmts <- newIORef id
     let onStmt s = modifyIORef stmts (. (s:))

         withDatabaseDebug _ q = Beam.withDatabaseDebug onStmt q
         putStrLn :: String -> IO ()
         putStrLn _ = pure ()
         print :: a -> IO ()
         print _ = pure ()
