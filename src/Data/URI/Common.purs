module Data.URI.Common where

import Prelude

import Control.Alt ((<|>))

import Data.Unfoldable (replicateA)
import Data.List (List)
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as Rx
import Data.Either (Either(..), fromRight)

import Text.Parsing.StringParser (Parser(..), ParseError(..), unParser)
import Text.Parsing.StringParser.String (string)
import Partial.Unsafe (unsafePartial)

joinWith ∷ String → List String → String
joinWith x y = S.joinWith x $ fromFoldable y

rep ∷ Int → Parser String → Parser String
rep n p = S.joinWith "" <$> replicateA n p

rxPat ∷ String → Parser String
rxPat rx = unsafePartial $ anyMatch $ fromRight $ Rx.regex rx (Rx.noFlags { ignoreCase = true })

wrapParser ∷ ∀ a. Parser a → Parser String → Parser a
wrapParser outer inner = Parser \ps → do
  { result: a, suffix: s } ← unParser inner ps
  { result: a', suffix: s' } ← unParser outer { str: a, pos: 0 }
  pure { result: a', suffix: s }

parsePChar ∷ Parser String
parsePChar
  = parseUnreserved
  <|> parsePCTEncoded
  <|> parseSubDelims
  <|> string ":"
  <|> string "@"

parseUnreserved ∷ Parser String
parseUnreserved = rxPat "[0-9a-z\\-\\._~]+"

parsePCTEncoded ∷ Parser String
parsePCTEncoded = rxPat "%[0-9a-f]{2}"

parseSubDelims ∷ Parser String
parseSubDelims = rxPat "[!$&'()*+;=]"

anyMatch ∷ Rx.Regex → Parser String
anyMatch rx = Parser \{ str: str, pos: i } → case match1From rx i str of
  Just m → Right { result: m, suffix: {str: str, pos: i + (S.length m) } }
  Nothing → Left { pos: i, error: ParseError $ "Expected " <> show rx }

match1From ∷ Rx.Regex → Int → String → Maybe String
match1From = match1FromImpl Just Nothing

foreign import match1FromImpl
  ∷ (∀ a. a → Maybe a)
  → (∀ a. Maybe a)
  → Rx.Regex
  → Int
  → String
  → (Maybe String)
