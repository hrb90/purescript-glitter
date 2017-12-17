module Glitter (glitter, glitterM, glitter', glitterM') where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic (class Generic)
import Text.Parsing.Parser (Parser, ParserT, parseErrorMessage, runParser, runParserT)
import Text.Parsing.Parser.String (class StringLike)

-- | Turns a `Parser String a` into a function `String -> Either String a` for consumption by `sparkle` 
glitter :: forall a. Generic a => Parser String a -> String -> Either String a
glitter = flip glitter' id

-- | Turns a `Parser s m a` into a function `s -> m (Either String a)` for consumption by `sparkle`, if `sparkle` can represent `m`. 
glitterM :: forall a s m. Generic a => StringLike s => Monad m => ParserT s m a -> s -> m (Either String a)
glitterM = flip glitterM' id

-- | Like `glitter`, but takes an extra argument transforming the parser output.
glitter' :: forall a b. Generic b => Parser String a -> (a -> b) -> String -> Either String b
glitter' parser evaluate = parser
                            # map evaluate
                            # flip runParser
                            # (map) (lmap parseErrorMessage)

-- | Like `glitterM`, but takes an extra argument transforming the parser output.
glitterM' :: forall a b s m. Generic b => StringLike s => Monad m => ParserT s m a -> (a -> b) -> s -> m (Either String b)
glitterM' parser evaluate = parser
                            # map evaluate
                            # flip runParserT
                            # (map <<< map) (lmap parseErrorMessage)