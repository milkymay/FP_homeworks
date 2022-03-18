{-# LANGUAGE DerivingVia #-}

module HW3.Action where

import Control.Exception.Base (Exception, throwIO)
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Set
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Time.Clock as C
import HW3.Base
import qualified System.Directory as D
import System.Random

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype PermissionException = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction (HiActionWrite p bs) =
    runWithAccess
      AllowWrite
      ( do
          B.writeFile p bs
          return HiValueNull
      )
  runAction (HiActionChDir p) =
    runWithAccess
      AllowRead
      ( do
          D.setCurrentDirectory p
          return HiValueNull
      )
  runAction (HiActionMkDir p) =
    runWithAccess
      AllowWrite
      ( do
          D.createDirectory p
          return HiValueNull
      )
  runAction (HiActionRead p) =
    runWithAccess
      AllowRead
      ( do
          isDir <- D.doesDirectoryExist p
          if isDir
            then do
              contains <- D.listDirectory p
              return $ HiValueList $ S.fromList $ fmap (HiValueString . T.pack) contains
            else do
              isFile <- D.doesFileExist p
              if isFile
                then do
                  bytes <- B.readFile p
                  case decodeUtf8' bytes of
                    Left _ -> return $ HiValueBytes bytes
                    Right content -> return $ HiValueString content
                else return HiValueNull
      )
  runAction HiActionCwd =
    runWithAccess AllowRead $ HiValueString . T.pack <$> D.getCurrentDirectory
  runAction HiActionNow =
    runWithAccess AllowTime $ HiValueTime <$> C.getCurrentTime
  runAction (HiActionRand a b) =
    HIO
      ( \_ -> do
          gen <- newStdGen
          return $ HiValueNumber $ toRational $ fst $ uniformR (a, b) gen
      )
  runAction (HiActionEcho text) =
    runWithAccess
      AllowWrite
      ( do
          putStrLn (T.unpack text)
          return HiValueNull
      )

runWithAccess :: HiPermission -> IO HiValue -> HIO HiValue
runWithAccess perm act =
  HIO
    ( \permissions ->
        if member perm permissions
          then act
          else throwIO $ PermissionRequired perm
    )
