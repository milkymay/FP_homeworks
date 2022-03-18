module HW3.Evaluator where

import Codec.Compression.Zlib
import Codec.Serialise
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Ratio (denominator)
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.Word
import HW3.Base
import Text.Read (readMaybe)
import Prelude

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . tryEval

tryLazyEval :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
tryLazyEval HiFunIf [cond, a, b] = do
  evalCond <- tryEval cond
  case evalCond of
    HiValueBool ans ->
      if ans
        then tryEval a
        else tryEval b
    _ -> throwE HiErrorInvalidArgument
tryLazyEval HiFunIf _ = throwE HiErrorArityMismatch
tryLazyEval HiFunOr [a, b] = do
  evalA <- tryEval a
  case evalA of
    HiValueNull -> tryEval b
    HiValueBool False -> tryEval b
    _ -> tryEval a
tryLazyEval HiFunOr _ = throwE HiErrorArityMismatch
tryLazyEval HiFunAnd [a, b] = do
  evalA <- tryEval a
  case evalA of
    HiValueNull -> return evalA
    HiValueBool False -> return evalA
    _ -> tryEval b
tryLazyEval HiFunAnd _ = throwE HiErrorArityMismatch
tryLazyEval _ _ = throwE HiErrorInvalidArgument

tryEval :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
tryEval (HiExprValue val) = return val
tryEval (HiExprApply fun args) =
  do
    val <- tryEval fun
    case val of
      HiValueFunction parsedFun ->
        case parsedFun of
          HiFunIf -> tryLazyEval HiFunIf args
          HiFunAnd -> tryLazyEval HiFunAnd args
          HiFunOr -> tryLazyEval HiFunOr args
          _ ->
            do
              argsVal <- mapM tryEval args
              handleFun parsedFun argsVal
      HiValueString str ->
        do
          argsVal <- mapM tryEval args
          handleIndexing (HiValueString str) argsVal
      HiValueList list ->
        do
          argsVal <- mapM tryEval args
          handleIndexing (HiValueList list) argsVal
      HiValueBytes bytes ->
        do
          argsVal <- mapM tryEval args
          handleIndexing (HiValueBytes bytes) argsVal
      _ -> throwE HiErrorInvalidFunction
tryEval (HiExprRun expr) = do
  evalVal <- tryEval expr
  case evalVal of
    HiValueAction act -> lift $ runAction act
    _ -> throwE HiErrorInvalidArgument

evalNum :: HiMonad m => HiValue -> ExceptT HiError m Rational
evalNum arg = do
  case arg of
    (HiValueNumber x) -> return x
    _ -> throwError HiErrorInvalidArgument

evalBool :: HiMonad m => HiValue -> ExceptT HiError m Bool
evalBool arg = do
  case arg of
    (HiValueBool x) -> return x
    _ -> throwError HiErrorInvalidArgument

evalString :: HiMonad m => HiValue -> ExceptT HiError m T.Text
evalString arg = do
  case arg of
    (HiValueString x) -> return x
    _ -> throwError HiErrorInvalidArgument

funLessThan, funGreaterThan, funEquals, funNotLessThan, funNotGreaterThan, funNotEquals :: HiValue -> HiValue -> HiValue
funLessThan a b = HiValueBool (a < b)
funGreaterThan a b = HiValueBool (a > b)
funEquals a b = HiValueBool (a == b)
funNotLessThan a b = HiValueBool (a >= b)
funNotGreaterThan a b = HiValueBool (a <= b)
funNotEquals a b = HiValueBool (a /= b)

transTypeAdd, transTypeMul, transTypeSub :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
transTypeAdd [HiValueString a, HiValueString b] = return (HiValueString (T.append a b))
transTypeAdd [HiValueList a, HiValueList b] = return (HiValueList (a <> b))
transTypeAdd [HiValueBytes a, HiValueBytes b] = return (HiValueBytes (a <> b))
transTypeAdd [HiValueTime a, HiValueNumber b] = return (HiValueTime (addUTCTime (fromRational b) a))
transTypeAdd args = handleFunction sum 2 HiValueNumber evalNum args
transTypeSub [HiValueTime a, HiValueTime b] = return (HiValueNumber (toRational (diffUTCTime a b)))
transTypeSub args = handleFunction (foldl1 (-)) 2 HiValueNumber evalNum args
transTypeMul [HiValueString a, HiValueNumber b] =
  tryMul a (HiValueNumber b) HiValueString
transTypeMul [HiValueList a, HiValueNumber b] =
  tryMul a (HiValueNumber b) HiValueList
transTypeMul [HiValueBytes a, HiValueNumber b] =
  tryMul a (HiValueNumber b) HiValueBytes
transTypeMul args = handleFunction product 2 HiValueNumber evalNum args

transTypeLength, transTypeReverse :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
transTypeLength [HiValueString a] = handleFunction (toRational . T.length . head) 1 HiValueNumber evalString [HiValueString a]
transTypeLength [HiValueList a] = return $ HiValueNumber $ toRational $ length a
transTypeLength [HiValueBytes a] = return $ HiValueNumber $ toRational $ B.length a
transTypeLength x = return $ HiValueNumber $ toRational $ length x
transTypeReverse [HiValueString a] = handleFunction (T.reverse . head) 1 HiValueString evalString [HiValueString a]
transTypeReverse [HiValueList a] = return $ HiValueList $ S.reverse a
transTypeReverse [HiValueBytes a] = return $ HiValueBytes $ B.reverse a
transTypeReverse _ = throwE HiErrorInvalidArgument

tryMul :: (HiMonad m, Semigroup a) => a -> HiValue -> (a -> HiValue) -> ExceptT HiError m HiValue
tryMul x (HiValueNumber b) f =
  if b <= 0 || b /= fromInteger (round b)
    then throwE HiErrorInvalidArgument
    else return $ f (stimes (round b :: Integer) x)
tryMul _ _ _ = throwE HiErrorInvalidArgument

handleFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
handleFun HiFunAdd = transTypeAdd
handleFun HiFunSub = transTypeSub
handleFun HiFunMul = transTypeMul
handleFun HiFunDiv = handleDiv
handleFun HiFunNot = handleFunction (not . head) 1 HiValueBool evalBool
handleFun HiFunAnd = handleFunction and 2 HiValueBool evalBool
handleFun HiFunOr = handleFunction or 2 HiValueBool evalBool
handleFun HiFunLessThan = handleFunction (foldl1 funLessThan) 2 id pure
handleFun HiFunGreaterThan = handleFunction (foldl1 funGreaterThan) 2 id pure
handleFun HiFunEquals = handleFunction (foldl1 funEquals) 2 id pure
handleFun HiFunNotLessThan = handleFunction (foldl1 funNotLessThan) 2 id pure
handleFun HiFunNotGreaterThan = handleFunction (foldl1 funNotGreaterThan) 2 id pure
handleFun HiFunNotEquals = handleFunction (foldl1 funNotEquals) 2 id pure
handleFun HiFunIf = handleIf
handleFun HiFunLength = transTypeLength
handleFun HiFunToUpper = handleFunction (T.toUpper . head) 1 HiValueString evalString
handleFun HiFunToLower = handleFunction (T.toLower . head) 1 HiValueString evalString
handleFun HiFunReverse = transTypeReverse
handleFun HiFunTrim = handleFunction (T.strip . head) 1 HiValueString evalString
handleFun HiFunList = handleListFunction S.fromList HiValueList
handleFun HiFunRange = handleRange
handleFun HiFunFold = handleFold
handleFun HiFunPackBytes = handlePack
handleFun HiFunUnpackBytes = handleUnpack
handleFun HiFunEncodeUtf8 = handleEncodeUtf8
handleFun HiFunDecodeUtf8 = handleDecodeUtf8
handleFun HiFunZip = handleZip
handleFun HiFunUnzip = handleUnzip
handleFun HiFunSerialise = handleSerialise
handleFun HiFunDeserialise = handleDeserialise
handleFun HiFunRead = handleRead
handleFun HiFunWrite = handleWrite
handleFun HiFunMkDir = handleMkDir
handleFun HiFunChDir = handleChDir
handleFun HiFunParseTime = handleParseTime
handleFun HiFunRand = handleRand
handleFun HiFunEcho = handleEcho

handleEcho :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleEcho [HiValueString text] = return $ HiValueAction $ HiActionEcho text
handleEcho args = checkArity [1] args

getNum :: HiMonad m => HiValue -> ExceptT HiError m Int
getNum a = case a of
  HiValueNumber x ->
    if denominator x == 1 && (x <= toRational (maxBound :: Int)) && (x >= toRational (minBound :: Int))
      then return $ fromEnum x
      else throwE HiErrorInvalidArgument
  _ -> throwE HiErrorInvalidArgument

handleRand :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleRand [a, b] = do
  l <- getNum a
  r <- getNum b
  if l > r
    then throwE HiErrorInvalidArgument
    else return $ HiValueAction $ HiActionRand (fromEnum l) (fromEnum r)
handleRand _ = throwE HiErrorArityMismatch

handleParseTime :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleParseTime [HiValueString time] = do
  case readMaybe $ T.unpack time of
    Nothing -> return HiValueNull
    Just res -> return $ HiValueTime res
handleParseTime args = checkArity [1] args

handleRead, handleWrite, handleMkDir, handleChDir :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleRead = handleAction HiActionRead
handleWrite [HiValueString p, HiValueBytes bytes] = return $ HiValueAction $ HiActionWrite (T.unpack p) bytes
handleWrite [HiValueString p, HiValueString str] =
  do
    text <- handleFun HiFunEncodeUtf8 [HiValueString str]
    case text of
      HiValueBytes bytes -> return $ HiValueAction $ HiActionWrite (T.unpack p) bytes
      _ -> throwE HiErrorInvalidArgument
handleWrite [_, _] = throwE HiErrorInvalidArgument
handleWrite _ = throwE HiErrorArityMismatch
handleMkDir = handleAction HiActionMkDir
handleChDir = handleAction HiActionChDir

handleAction :: HiMonad m => (FilePath -> HiAction) -> [HiValue] -> ExceptT HiError m HiValue
handleAction act [HiValueString p] = return $ HiValueAction $ act (T.unpack p)
handleAction _ args = checkArity [1] args

handleDiv :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleDiv [HiValueNumber a, HiValueNumber b] =
  if b == 0
    then throwE HiErrorDivideByZero
    else return $ HiValueNumber (a / b)
handleDiv [HiValueString a, HiValueString b] = return $ HiValueString (T.concat [a, T.pack "/", b])
handleDiv args = checkArity [2] args

handleIf :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleIf [HiValueBool cond, a, b] = if cond then return a else return b
handleIf args = checkArity [3] args

handleSerialise, handleDeserialise :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleSerialise [val] = return $ HiValueBytes $ BL.toStrict $ serialise val
handleSerialise _ = throwE HiErrorInvalidArgument
handleDeserialise [HiValueBytes bytes] = return $ deserialise $ BL.fromStrict bytes
handleDeserialise _ = throwE HiErrorInvalidArgument

handleZip, handleUnzip :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleZip = handleZipUnzip compressWith defaultCompressParams {compressLevel = bestCompression}
handleUnzip = handleZipUnzip decompressWith defaultDecompressParams

handleZipUnzip :: HiMonad m => (a -> BL.ByteString -> BL.ByteString) -> a -> [HiValue] -> ExceptT HiError m HiValue
handleZipUnzip f params [HiValueBytes bytes] =
  return $
    HiValueBytes $
      BL.toStrict $
        f params (BL.fromStrict bytes)
handleZipUnzip _ _ _ = throwE HiErrorInvalidArgument

handlePack, handleUnpack :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handlePack [HiValueList args] =
  do
    bytes <- mapM ratToWord8 args
    return $ HiValueBytes $ B.pack (toList bytes)
handlePack [HiValueBytes bytes] = return $ HiValueBytes bytes
handlePack _ = throwE HiErrorInvalidArgument
handleUnpack [HiValueBytes bytes] = return $ HiValueList $ fmap (HiValueNumber . fromInteger . toInteger) (S.fromList (B.unpack bytes))
handleUnpack _ = throwE HiErrorInvalidArgument

ratToWord8 :: HiMonad m => HiValue -> ExceptT HiError m Word8
ratToWord8 (HiValueNumber val) =
  if (denominator val /= 1) || (val < 0) || (val > 255)
    then throwE HiErrorInvalidArgument
    else return $ fromInteger $ truncate val
ratToWord8 _ = throwE HiErrorInvalidArgument

handleEncodeUtf8, handleDecodeUtf8 :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleEncodeUtf8 [HiValueString str] = return $ HiValueBytes (encodeUtf8 str)
handleEncodeUtf8 _ = throwE HiErrorInvalidArgument
handleDecodeUtf8 [HiValueBytes bytes] =
  case decodeUtf8' bytes of
    Right res -> return $ HiValueString res
    Left _ -> return HiValueNull
handleDecodeUtf8 _ = throwE HiErrorInvalidArgument

handleRange :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleRange [HiValueNumber a, HiValueNumber b] = return $ HiValueList $ S.fromList $ map HiValueNumber [a .. b]
handleRange args = checkArity [2] args

handleFold :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
handleFold [HiValueFunction f, HiValueList b] = do
  if null b
    then return HiValueNull
    else
      foldM
        (\acc el -> tryEval $ HiExprApply (HiExprValue (HiValueFunction f)) [HiExprValue acc, HiExprValue el])
        (S.index b 0)
        (S.drop 1 b)
handleFold args = checkArity [2] args

handleFunction ::
  HiMonad m =>
  ([a] -> b) ->
  Int ->
  (b -> HiValue) ->
  (HiValue -> ExceptT HiError m a) ->
  [HiValue] ->
  ExceptT HiError m HiValue
handleFunction fun arity cast evalFun args =
  if arity == length args
    then do
      evalVal <- mapM evalFun args
      return $ cast (fun evalVal)
    else throwE HiErrorArityMismatch

handleListFunction ::
  HiMonad m =>
  ([HiValue] -> b) ->
  (b -> HiValue) ->
  [HiValue] ->
  ExceptT HiError m HiValue
handleListFunction fun cast args =
  return $ cast (fun args)

checkBoundary :: HiMonad m => HiValue -> ExceptT HiError m Int
checkBoundary a = do
  case a of
    HiValueNumber x ->
      if x == fromInteger (round x :: Integer)
        then return (fromEnum x)
        else throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument

handleIndexing :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
handleIndexing (HiValueList list) [a, b] =
  do
    args <- checkBoundaries (S.length list) a b
    case args of
      Nothing -> return HiValueNull
      Just (start, len) -> return $ HiValueList $ S.take len (S.drop start list)
handleIndexing (HiValueList list) [a] = do
  ind <- checkBoundary a
  if (0 <= ind) && (ind < S.length list)
    then return $ S.index list ind
    else return HiValueNull
handleIndexing (HiValueString str) [a, b] = do
  args <- checkBoundaries (T.length str) a b
  case args of
    Nothing -> return $ HiValueString $ T.pack ""
    Just (start, len) -> return $ HiValueString $ T.take len (T.drop start str)
handleIndexing (HiValueString str) [a] = do
  ind <- checkBoundary a
  if (0 <= ind) && (ind < T.length str)
    then return $ HiValueString $ T.pack [T.index str ind]
    else return HiValueNull
handleIndexing (HiValueBytes str) [a, b] =
  do
    args <- checkBoundaries (B.length str) a b
    case args of
      Nothing -> return HiValueNull
      Just (start, len) -> return $ HiValueBytes $ B.take len (B.drop start str)
handleIndexing (HiValueBytes str) [a] = do
  ind <- checkBoundary a
  if (0 <= ind) && (ind < B.length str)
    then return $ HiValueNumber $ toRational $ B.index str ind
    else return HiValueNull
handleIndexing _ args = checkArity [1, 2] args

checkBoundaries :: HiMonad m => Int -> HiValue -> HiValue -> ExceptT HiError m (Maybe (Int, Int))
checkBoundaries len l r =
  do
    let left = do
          case l of
            HiValueNumber a ->
              if a < 0
                then return $ len + (fromEnum a :: Int)
                else return (fromEnum a :: Int)
            HiValueNull -> return 0
            _ -> throwE HiErrorInvalidArgument
    let right = do
          case r of
            HiValueNumber a ->
              if a < 0
                then return $ len + (fromEnum a :: Int)
                else return (fromEnum a :: Int)
            HiValueNull -> return len
            _ -> throwE HiErrorInvalidArgument
    start <- left
    end <- right
    if end < start
      then return Nothing
      else return $ Just (start, end - start)

checkArity :: HiMonad m => [Int] -> [HiValue] -> ExceptT HiError m HiValue
checkArity n acc =
  if length acc `elem` n
    then throwE HiErrorInvalidArgument
    else throwE HiErrorArityMismatch
