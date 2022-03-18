module Main where

import System.Console.Haskeline

import HW3.Parser
import HW3.Evaluator
import HW3.Pretty
import HW3.Action
import Data.Set
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.IO.Class

permissions :: Set HiPermission
permissions = fromList [AllowRead, AllowWrite, AllowTime]

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "q" -> return ()
        Just input -> do
          let parsed = parse input
          outputStrLn $ show parsed
          case parsed of
            Left parseError -> outputStrLn $ errorBundlePretty parseError
            Right result -> do
              evaluated <- liftIO $ runHIO (eval result) permissions
              case evaluated of
                Left evalError -> outputStrLn $ show evalError
                Right evalResult -> outputStrLn $ show $ prettyValue evalResult
          loop