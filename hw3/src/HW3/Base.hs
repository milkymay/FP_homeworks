{-# LANGUAGE DeriveGeneric #-}

module HW3.Base where

import Codec.Serialise
import Data.ByteString.Char8 (ByteString)
import Data.Sequence
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data HiFun -- function names (e.g. div, sort, length, ...)
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | --
    HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | --
    HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | --
    HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | --
    HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | --
    HiFunParseTime
  | --
    HiFunRand
  | --
    HiFunEcho
  deriving (Show, Eq, Ord, Generic)

data HiValue -- values (numbers, booleans, strings, ...)
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  deriving (Show, Eq, Ord, Generic)

data HiExpr -- expressions (literals, function calls, ...)
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  deriving (Show, Eq, Ord, Generic)

data HiError -- evaluation errors (invalid arguments, ...)
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiFun

instance Serialise HiValue

instance Serialise HiAction

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
