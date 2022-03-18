module HW3.Pretty where

import qualified Data.ByteString as B
import Data.Scientific
import Data.Word
import GHC.Real (denominator, numerator)
import HW3.Base
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction fun) =
  case fun of
    HiFunAdd -> pretty "add"
    HiFunSub -> pretty "sub"
    HiFunMul -> pretty "mul"
    HiFunDiv -> pretty "div"
    HiFunNot -> pretty "not"
    HiFunAnd -> pretty "and"
    HiFunOr -> pretty "or"
    HiFunLessThan -> pretty "less-than"
    HiFunGreaterThan -> pretty "greater-than"
    HiFunEquals -> pretty "equals"
    HiFunNotLessThan -> pretty "not-less-than"
    HiFunNotGreaterThan -> pretty "not-greater-than"
    HiFunNotEquals -> pretty "not-equals"
    HiFunIf -> pretty "if"
    HiFunLength -> pretty "length"
    HiFunToUpper -> pretty "to-upper"
    HiFunToLower -> pretty "to-lower"
    HiFunReverse -> pretty "reverse"
    HiFunTrim -> pretty "trim"
    HiFunList -> pretty "list"
    HiFunFold -> pretty "fold"
    HiFunRange -> pretty "range"
    HiFunPackBytes -> pretty "pack-bytes"
    HiFunUnpackBytes -> pretty "unpack-bytes"
    HiFunZip -> pretty "zip"
    HiFunUnzip -> pretty "unzip"
    HiFunEncodeUtf8 -> pretty "encode-utf8"
    HiFunDecodeUtf8 -> pretty "decode-utf8"
    HiFunSerialise -> pretty "serialise"
    HiFunDeserialise -> pretty "deserialise"
    HiFunRead -> pretty "read"
    HiFunWrite -> pretty "write"
    HiFunMkDir -> pretty "mkdir"
    HiFunChDir -> pretty "cd"
    HiFunParseTime -> pretty "parse-time"
    HiFunRand -> pretty "rand"
    HiFunEcho -> pretty "echo"
prettyValue (HiValueNumber num) =
  let numVal = numerator num
      denomVal = denominator num
      (val, remain) = quotRem numVal denomVal
   in if denomVal == 1
        then pretty numVal
        else case fromRationalRepetendUnlimited num of
          (res, Nothing) -> pretty $ formatScientific Fixed Nothing res
          _ ->
            if val == 0
              then foldMap pretty [show numVal, "/", show denomVal]
              else
                if numVal > 0
                  then foldMap pretty [show val, " + ", show remain, "/", show denomVal]
                  else foldMap pretty [show val, " - ", show (abs remain), "/", show denomVal]
prettyValue (HiValueBool bool) = if bool then pretty "true" else pretty "false"
prettyValue (HiValueString str) = viaShow str
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList args) =
  if null args
    then pretty "[ ]"
    else
      pretty "[ "
        <> foldl1 (\acc a -> acc <> pretty ", " <> a) (fmap prettyValue args)
        <> pretty " ]"
prettyValue (HiValueBytes bytes) =
  pretty "[#"
    <> foldMap prettyPrecision (B.unpack bytes)
    <> pretty " #]"
prettyValue (HiValueAction act) =
  case act of
    HiActionRead fp -> pretty "read(" <> viaShow fp <> pretty ")"
    HiActionWrite fp bs -> pretty "write(" <> viaShow fp <> pretty ", " <> prettyValue (HiValueBytes bs) <> pretty ")"
    HiActionMkDir fp -> pretty "mkdir(" <> viaShow fp <> pretty ")"
    HiActionChDir fp -> pretty "cd(" <> viaShow fp <> pretty ")"
    HiActionCwd -> pretty "cwd"
    HiActionNow -> pretty "now"
    HiActionRand a b -> pretty "rand(" <> pretty (show a) <> pretty ", " <> pretty (show b) <> pretty ")"
    HiActionEcho text -> pretty "echo(" <> viaShow text <> pretty ")"
prettyValue (HiValueTime time) = pretty "parse-time(\"" <> viaShow time <> pretty "\")"

prettyPrecision :: Word8 -> Doc AnsiStyle
prettyPrecision byte = foldMap pretty [" ", if byte < 16 then "0" else "", showHex byte ""]
