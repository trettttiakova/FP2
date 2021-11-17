{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
module HW2.T6
  ( ParseError (..)
  , parseError
  , parseExpr
  , pChar
  , pEof
  , runP
  ) where

import Control.Applicative ( Alternative(some, (<|>), empty, many) )
import Data.Char ( isDigit, digitToInt, isSpace )
import Data.Scientific ( scientific, toRealFloat )
import Control.Monad (mfilter, MonadPlus)
import GHC.Natural ( Natural )
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (Op, Val), Prim (Mul, Div, Add, Sub))
import HW2.T5 ( ExceptState(..) )
import Data.Foldable
import GHC.Float (int2Double)

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- |Parses the given string with the given parser.
runP :: Parser a -> String -> Except ParseError a
runP (P es) str = extractErrorOrSuccess $ runES es (0, str)
  where
    extractErrorOrSuccess (Error e)          = Error e
    extractErrorOrSuccess (Success (a :# _)) = Success a

-- |pChar is a parser that consumes one char.
-- When it is run (using runP) with empty string as input,
-- it will return error with error message (ErrorAtPos 0).
-- When it is run with non empty string as input,
-- it will return Success with the consumed char
-- annotated with next position's number and a remaining string.
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- |Parser that always fails.
parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty                       = parseError
  p@(P pstate) <|> (P qstate) = P $ ES \(pos, s) -> case runP p s of
    (Success _) -> runES pstate (pos, s)
    (Error _)   -> runES qstate (pos, s)

instance MonadPlus Parser

-- |Parser that succeeds if the string is empty (end of input),
-- fails otherwise.
pEof :: Parser ()
pEof = P $ ES \(pos, s) -> case s of
  [] -> Success (() :# (pos + 1, s))
  _  -> Error (ErrorAtPos pos)

pDot :: Parser ()
pDot = P $ ES \(pos, s) -> case s of
  ('.':cs) -> Success (() :# (pos + 1, cs))
  _        -> Error (ErrorAtPos pos)

-- |Takes two arguments: integer accumulator and string.
stringToInteger :: Integer -> String -> Integer
stringToInteger = foldl' (\acc d -> acc * 10 + toInteger (digitToInt d))

pOpenBracket :: Parser ()
pOpenBracket = P $ ES \(pos, s) -> case s of
  ('(':cs) -> Success (() :# (pos + 1, cs))
  _        -> Error (ErrorAtPos pos)

pCloseBracket :: Parser ()
pCloseBracket = P $ ES \(pos, s) -> case s of
  (')':cs) -> Success (() :# (pos + 1, cs))
  _        -> Error (ErrorAtPos pos)

pParenthesis :: Parser Expr
pParenthesis = do
  pOpenBracket
  expr <- pExpression
  pCloseBracket
  pure expr

pDouble :: Parser Expr
pDouble = do
  first <- some (mfilter Data.Char.isDigit pChar)
  pDot
  second <- some (mfilter Data.Char.isDigit pChar)
  let integralPart = stringToInteger 0 first
  let bothParts    = stringToInteger integralPart second
  pure $ Val $ toRealFloat $ scientific bothParts (- length second)

pInteger :: Parser Expr
pInteger = do
  first <- some (mfilter Data.Char.isDigit pChar)
  let value = stringToInteger 0 first
  pure $ Val $ toRealFloat $ scientific value 0

pNumber :: Parser Expr
pNumber = pDouble <|> pInteger

pWhiteSpace :: Parser ()
pWhiteSpace = P $ ES \(pos, s) -> case s of
  (c:cs) -> if isSpace c
    then
      Success (() :# (pos + 1, cs))
    else Error (ErrorAtPos pos)
  _      -> Error (ErrorAtPos pos)

pHighPriorityOperator :: Parser Char
pHighPriorityOperator = P $ ES \(pos, s) -> case s of
  ('*':cs) -> Success ('*' :# (pos + 1, cs))
  ('/':cs) -> Success ('/' :# (pos + 1, cs))
  _        -> Error (ErrorAtPos pos)

getExprForOperator :: Char -> Expr -> Expr -> Expr
getExprForOperator '*' y x = Op $ Mul x y
getExprForOperator '/' y x = Op $ Div x y
getExprForOperator '-' y x = Op $ Sub x y
getExprForOperator '+' y x = Op $ Add x y
getExprForOperator _ _ _   = undefined

pHighPriority :: Parser Expr
pHighPriority = do
  x    <- many pWhiteSpace *> (pNumber <|> pParenthesis)
  expr <- many pHighPriority'
  pure $ foldl' (flip id) x expr
    where
      -- |pHighPriority' returns a function that takes an argument 
      -- and depending on the operator returns Mul or Div with
      -- the the given argument as the first value.
      -- Example:
      -- "many pHighPriority'" for *5/10 will result in
      -- [\x -> Op (Mul x 5), \x -> Op (Div x 10)].
      pHighPriority' = do
        operator <- many pWhiteSpace *> pHighPriorityOperator
        y        <- many pWhiteSpace *> (pNumber <|> pParenthesis)
        pure $ getExprForOperator operator y

pLowPriorityOperator :: Parser Char
pLowPriorityOperator = P $ ES \(pos, s) -> case s of
  ('+':cs) -> Success ('+' :# (pos + 1, cs))
  ('-':cs) -> Success ('-' :# (pos + 1, cs))
  _        -> Error (ErrorAtPos pos)

pLowPriority :: Parser Expr
pLowPriority = do
  x    <- many pWhiteSpace *> pHighPriority
  expr <- many pLowPriority'
  pure $ foldl' (flip id) x expr
    where
      pLowPriority' = do
        operator <- many pWhiteSpace *> pLowPriorityOperator
        y        <- many pWhiteSpace *> pHighPriority
        pure $ getExprForOperator operator y

-- |Main parser that parses the whole expression.
pExpression :: Parser Expr
pExpression = do
  expr <- pLowPriority
  _ <- many pWhiteSpace
  pEof
  pure expr

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpression