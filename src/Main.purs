module Glitter (glit, glitM, glitter, glit', glitM', glitter') where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic (class Generic)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)
import Text.Parsing.Parser (Parser, ParserT, parseErrorMessage, runParser, runParserT)
import Text.Parsing.Parser.String (class StringLike)

-- | Turns a `Parser String a` into a function `String -> Either String a` for consumption by `sparkle` 
glit :: forall a. Generic a => Parser String a -> String -> Either String a
glit = flip glit' id

-- | Turns a `Parser s m a` into a function `s -> m (Either String a)` for consumption by `sparkle`, if `sparkle` can represent `m`. 
glitM :: forall a s m. Generic a => StringLike s => Monad m => ParserT s m a -> s -> m (Either String a)
glitM = flip glitM' id

glitter :: forall a eff.                                
  Generic a => String                        
                 -> Parser String a
                    -> Eff                     
                         ( channel :: CHANNEL  
                         , dom :: DOM          
                         | eff                 
                         )                     
                         Unit
glitter label = flip (glitter' label) id

-- | Like `glitter`, but takes an extra argument transforming the parser output.
glit' :: forall a b. Generic b => Parser String a -> (a -> b) -> String -> Either String b
glit' parser evaluate = parser
                            # map evaluate
                            # flip runParser
                            # (map) (lmap parseErrorMessage)

-- | Like `glitterM`, but takes an extra argument transforming the parser output.
glitM' :: forall a b s m. Generic b => StringLike s => Monad m => ParserT s m a -> (a -> b) -> s -> m (Either String b)
glitM' parser evaluate = parser
                            # map evaluate
                            # flip runParserT
                            # (map <<< map) (lmap parseErrorMessage)


glitter' :: forall a b eff.                             
  Generic b => String                         
                 -> Parser String a 
                    -> (a -> b)             
                       -> Eff                   
                            ( channel :: CHANNEL
                            , dom :: DOM        
                            | eff               
                            )                   
                            Unit
glitter' label parser evaluate = sparkle label $ glit' parser evaluate