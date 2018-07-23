module Database.Beam.Migrate.Tool.Branch where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry

import           Control.Monad

#if !MIN_VERSION_base(4, 11, 0)
import           Data.Monoid
#endif
import qualified Data.Text as T

import           System.Console.ANSI

listBranches :: MigrateCmdLine -> IO ()
listBranches cmdLine = do
  reg <- lookupRegistry cmdLine

  case migrationRegistryHead reg of
    MigrationHeadDetached commitId -> do
      setSGR [ SetColor Foreground Dull Green ]
      putStrLn ("(Currently in detached HEAD mode at " ++ show commitId ++ ")")
      setSGR [ Reset ]
    _ -> pure ()

  forM_ (migrationRegistryBranches reg) $ \branch -> do
    let isCurrent = migrationRegistryHead reg == MigrationHeadBranch (migrationBranchName branch)
    putStrLn ((if isCurrent then "* " <> setSGRCode [ SetColor Foreground Dull Green ] else "  ") <>
              T.unpack (migrationBranchName branch) <>
              setSGRCode [ Reset ])

deleteBranch :: MigrateCmdLine -> T.Text -> IO ()
deleteBranch cmdLine branchNm =
  updatingRegistry cmdLine $ \reg ->
  pure ((), reg { migrationRegistryBranches =
                    filter (\branch -> migrationBranchName branch /= branchNm)
                           (migrationRegistryBranches reg) })

newBranch :: MigrateCmdLine -> Bool -> T.Text -> IO ()
newBranch cmdLine dontSwitch branchNm =
  updatingRegistry cmdLine $ \reg ->
  case lookupBranch reg branchNm of
    Nothing ->
      pure ((), reg { migrationRegistryBranches = MigrationBranch branchNm (registryHeadCommit reg):migrationRegistryBranches reg
                    , migrationRegistryHead = if dontSwitch then migrationRegistryHead reg else MigrationHeadBranch branchNm } )
    Just _ -> fail "Branch already exists"
