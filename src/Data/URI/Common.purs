module Data.URI.Common where

import Prelude

import Control.Alt ((<|>))

import Data.Unfoldable (replicateA)
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.Regex as Rx
import Data.Either (Either(..))

import Text.Parsing.StringParser (Parser(..), ParseError(..), unParser)
import Text.Parsing.StringParser.String (string)

joinWith ∷ String → List String → String
joinWith x y = S.joinWith x $ toUnfoldable y

rep ∷ Int → Parser String → Parser String
rep n p = S.joinWith "" <$> replicateA n p

rxPat ∷ String → Parser String
rxPat rx = anyMatch $ Rx.regex rx (Rx.noFlags { ignoreCase = true })

-- before
-- data Parser a = Parser (forall r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r)
-- unParser :: forall a r. Parser a -> PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r

-- wrapParser ∷ ∀ a. Parser a → Parser String → Parser a
-- wrapParser outer inner = Parser \ps fc sc →
--   unParser inner ps fc (\s ps' →
--     unParser outer { str: s, pos: 0 } fc (\s' _ → sc s' ps'))

-- now
-- data Parser a = Parser (PosString -> Either { pos :: Pos, error :: ParseError } { result :: a, suffix :: PosString })
-- unParser :: forall a. Parser a -> PosString -> Either { pos :: Pos, error :: ParseError } { result :: a, suffix :: PosString }
-- PosString :: { str :: String, pos :: Pos }
-- temporary just to make it build.
wrapParser ∷ ∀ a. Parser a → Parser String → Parser a
wrapParser outer inner = Parser \ps → do
  { result: a, suffix: s } ← unParser inner ps
  { result: a', suffix: s' } ← unParser outer { str: a, pos: 0 }
  pure { result: a', suffix: s' }

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

anyMatch ∷ Either String Rx.Regex → Parser String
-- anyMatch rx = Parser \{ str: str, pos: i } fc sc → case match1From rx i str of
--   Just m → sc m { str: str, pos: i + (S.length m) }
--   Nothing → fc i (ParseError $ "Expected " <> show rx)
anyMatch rx = Parser \{ str: str, pos: i } → case match1From rx i str of
  Just m → Right { result: m, suffix: {str: str, pos: i + (S.length m) } }
  Nothing → Left { pos: i, error: ParseError $ "Expected " <> show rx }

-- I'm discarding the error. Should probably be improved.
match1From ∷ Either String Rx.Regex → Int → String → Maybe String
match1From rx i str = case rx of
  (Left _) → Nothing
  (Right r) → match1FromImpl Just Nothing r i str

foreign import match1FromImpl
  ∷ (∀ a. a → Maybe a)
  → (∀ a. Maybe a)
  → Rx.Regex
  → Int
  → String
  → (Maybe String)
