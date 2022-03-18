{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Control.Monad.Combinators.Expr
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (charLiteral)
import qualified Text.Megaparsec.Char.Lexer as L

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = Text.Megaparsec.parse (hiOperatorsParser <* handleWS <* eof) ""

type Parser = Parsec Void String

funParser :: Parser HiFun
funParser =
  choice
    [ HiFunAdd <$ string "add",
      HiFunMul <$ string "mul",
      HiFunSub <$ string "sub",
      HiFunDiv <$ string "div",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNotEquals <$ string "not-equals",
      HiFunNot <$ string "not",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunReverse <$ string "reverse",
      HiFunTrim <$ string "trim",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold",
      HiFunPackBytes <$ string "pack-bytes",
      HiFunUnpackBytes <$ string "unpack-bytes",
      HiFunZip <$ string "zip",
      HiFunUnzip <$ string "unzip",
      HiFunEncodeUtf8 <$ string "encode-utf8",
      HiFunDecodeUtf8 <$ string "decode-utf8",
      HiFunSerialise <$ string "serialise",
      HiFunDeserialise <$ string "deserialise",
      HiFunRead <$ string "read",
      HiFunWrite <$ string "write",
      HiFunMkDir <$ string "mkdir",
      HiFunChDir <$ string "cd",
      HiFunParseTime <$ string "parse-time",
      HiFunRand <$ string "rand",
      HiFunEcho <$ string "echo"
    ]

ratParser :: Parser Rational
ratParser = toRational <$> L.signed handleWS L.scientific

boolParser :: Parser HiValue
boolParser =
  do
    handleWS
    choice
      [ HiValueBool True <$ string "true",
        HiValueBool False <$ string "false"
      ]

actParser :: Parser HiValue
actParser =
  do
    handleWS
    HiValueAction HiActionCwd <$ string "cwd"

timeParser :: Parser HiValue
timeParser =
  do
    handleWS
    HiValueAction HiActionNow <$ string "now"

nullParser :: Parser HiValue
nullParser =
  do
    handleWS
    HiValueNull <$ string "null"

textParser :: Parser T.Text
textParser = T.pack <$> (string "\"" *> manyTill charLiteral (string "\""))

byteParser :: Parser B.ByteString
byteParser = between (string "[#") (string "#]") bP

bP :: Parser B.ByteString
bP =
  B.pack
    <$> ( do
            handleWS
            args <- sepEndBy parseByte (string " ")
            handleWS
            return args
        )

parseByte :: Parser Word8
parseByte = do
  a <- C.hexDigitChar
  b <- C.hexDigitChar
  return $ fromInteger $ toInteger $ 16 * digitToInt a + digitToInt b

handleWS :: Parser ()
handleWS = L.space space1 empty empty

hiValParser :: Parser HiValue
hiValParser =
  do
    handleWS
    choice
      [ HiValueNumber <$> ratParser,
        HiValueFunction <$> funParser,
        boolParser,
        nullParser,
        HiValueString <$> textParser,
        HiValueBytes <$> byteParser,
        timeParser,
        actParser
      ]

hiExprsParser :: Parser [HiExpr]
hiExprsParser = do
  handleWS
  args <- sepBy hiOperatorsParser (string ",")
  handleWS
  return args

hiExprParser :: Parser HiExpr
hiExprParser =
  choice
    [ do
        handleWS
        hiValue <- hiValParser
        hiExprParser' (HiExprValue hiValue),
      do
        args <- between (string "[") (string "]") hiExprsParser
        hiExprParser' $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) args
    ]

hiExprParser' :: HiExpr -> Parser HiExpr
hiExprParser' valOrApply =
  do
    handleWS
    choice
      [ do
          exprs <- parens hiExprsParser
          hiExprParser' (HiExprApply valOrApply exprs),
        do
          _ <- string "!"
          handleWS
          hiExprParser' (HiExprRun valOrApply),
        return valOrApply
      ]

hiOperatorsParser :: Parser HiExpr
hiOperatorsParser = makeExprParser hiTermsParser operatorTable

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

hiTermsParser :: Parser HiExpr
hiTermsParser =
  do
    handleWS
    res <-
      choice
        [ hiExprParser,
          do
            expr <- parens hiOperatorsParser
            hiExprParser' expr
        ]
    handleWS
    return res

parseDiv :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
parseDiv parser f = InfixL (f <$ parser)

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ parseDiv (try (string "/" <* notFollowedBy "=")) (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) [a, b]),
      binaryL "*" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [a, b])
    ],
    [ binaryL "+" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [a, b]),
      binaryL "-" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [a, b])
    ],
    [ binaryN "<" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) [a, b]),
      binaryN ">" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) [a, b]),
      binaryN ">=" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) [a, b]),
      binaryN "<=" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) [a, b]),
      binaryN "==" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [a, b]),
      binaryN "/=" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) [a, b])
    ],
    [binaryR "&&" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) [a, b])],
    [binaryR "||" (\a b -> HiExprApply (HiExprValue (HiValueFunction HiFunOr)) [a, b])]
  ]

binaryL, binaryR, binaryN :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryL name f = InfixL (f <$ string name)
binaryR name f = InfixR (f <$ string name)
binaryN name f = InfixN (f <$ string name)
