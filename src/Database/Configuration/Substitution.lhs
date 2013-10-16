Expansion of variables in a string.

> {-# LANGUAGE RankNTypes  #-}
> module Database.Configuration.Substitution(substitution, Data) where
> import qualified Data.Set as Set
> import Data.Functor.Identity (Identity)
> import Control.Applicative hiding ((<|>), many, optional)
> import Text.Parsec.Perm()
> import Text.Parsec.Pos()
> import Text.Parsec.Prim
> import Text.Parsec.Char
> import Text.Parsec.Combinator
> import Database.Configuration.Types

This is a fairly straight-forward chunk of code that replaces variable names in a string, recursively. Invalid parses are included as raw strings. We detect and prevent recursive expansions by keeping a list of keys we're expanding. 

> substitution :: (Key -> Maybe Data) -> Data -> Data
> substitution = substitutionInner Set.empty
> substitutionInner :: Set.Set Key -> (Key -> Maybe Data) -> Data -> Data
> substitutionInner s f x = case parse varParser "" x of
>                       Left  _  -> x
>                       Right pt -> doSubs s f =<< pt
>   where doSubs _  _  (Raw r) = r
>         doSubs ss ff (Var v) = if Set.notMember v ss
>                                then case ff v of 
>                                        Just str -> substitutionInner (Set.insert v ss) ff str
>                                        Nothing  -> ""
>                                else ""

Now we need our parser, but first let's define our basic datatype that we're trying to generate as the output:

> data PartType = Raw Data | Var Data

And then, the parser in its entirety:

> varParser = many (escapeSequence <|> variableRef <|> rawString)
> escapeSequence = Raw "$" <$ try (string "$$")
> variableRef = Var <$> (char '$' *> between (char '{') (char '}') (many1 (noneOf "${}")))
> rawString = Raw <$> many1 (noneOf "$")


Provide some type definitions to suppress compiler warnings:

> varParser :: forall u. ParsecT Data u Identity [PartType]
> escapeSequence, variableRef, rawString :: forall u. ParsecT Data u Identity PartType

