{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

{-| This library only exports a single `dhallToGraphQL` function for translating a
    Dhall syntax tree to a GraphQL SDL string.

    See the @dhall@ package if you would like to transform Dhall source code
    into a Dhall syntax tree.

    This package also provides a @dhall-to-graphql@ executable which you can
    use to compile Dhall source code directly to GraphQL for your convenience.

    Not all Dhall expressions can be converted to GraphQL since the GraphQL SDL
    is not a programming language.  The only things you can convert are:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@
    * @List@s
    * @Optional@ values
    * unions
    * records

    Dhall @Bool@s translate to GraphQL bools:

> $ dhall-to-graphql <<< 'Bool'
> Boolean!

    Dhall number types are translated to the GraphQL equivalent:

> $ dhall-to-graphql <<< 'Natural'
> Int!
> $ dhall-to-graphql <<< 'Integer'
> Int!
> $ dhall-to-graphql <<< 'Double'
> Float!

    Dhall @Text@ translates to GraphQL strings:

> $ dhall-to-graphql <<< 'Text'
> String!

    Dhall @List@s translate to GraphQL lists:

> $ dhall-to-graphql <<< '[1, 2, 3] : List Natural'
> [Integer!]!

    Dhall @Optional@ values translate to @nullable@ if absent and the unwrapped
    value otherwise:

> $ dhall-to-graphql <<< 'Optional Natural'
> Int
> $ dhall-to-graphql <<< 'Optional Text'
> String

    Dhall records translate to GraphQL objects:

> $ dhall-to-graphql <<< '{ __gqlName = "Foo", bar : Natural, baz = Bool }'
> type Foo {
>   "bar": Int!
>   "baz": Boolean!
> }

    Dhall unions translate to the wrapped value:

> $ dhall-to-graphql <<< "{ __gqlName = "Either", __gqlUnion = < Left : Natural | Right : Natural> }"
> type EitherLeft { value = Int! }
> type EitherRight { value = Int! }
> union Either = EitherLeft | EitherRight

> $ cat graphql
> let MyType =
>       < Person : { age : Natural, name : Text } | Place : { location : Text } >
>
> let ProperNoun =
>       { __gqlName = "ProperNoun", __gqlUnion = MyType }
>
> let query =
    { __gqlName = "Query", properNouns : List ProperNoun }
>
> in query

> $ dhall-to-graphql <<< "./config"
> type ProperNounPerson {
>   age: Int!
>   name: String!
> }
>
> type ProperNounPlace {
>   location: String!
> }
>
> union ProperNoun = ProperNounPerson | ProperNounPlace
>
> type Query {
>   properNouns: [ProperNoun!]!
> }
-}

module Dhall.GraphQL
  (
    -- * Dhall to GraphQL
    dhallToGraphQL
    -- * Exceptions
  , CompileError(..)
  )
where

import           Control.Applicative            ( empty
                                                , (<|>)
                                                )
import           Control.Monad                  ( guard )
import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>)
                                                , mempty
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc      ( Pretty )
import           Data.Void                      ( Void )
import           Dhall.Core                     ( Binding(..)
                                                , DhallDouble(..)
                                                , Expr
                                                )
import           Dhall.Import                   ( SemanticCacheMode(..) )
import           Dhall.Map                      ( Map )
import           Options.Applicative            ( Parser )
import           Prelude                 hiding ( getContents )

import qualified Data.Foldable                 as Foldable
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.List
import qualified Data.Map
import qualified Data.Ord
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as Pretty
import qualified Data.Vector                   as Vector
import qualified Dhall.Core                    as Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Optics
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Util
import qualified Options.Applicative
import qualified System.FilePath

{-| This is the exception type for errors that might arise when translating
    Dhall to GraphQL

    Because the majority of Dhall language features do not translate to GraphQL
    this just returns the expression that failed
-}
data CompileError
    = Unsupported (Expr Void Void)
    | BareNone

instance Show CompileError where
  show BareNone =
    Data.Text.unpack
      $ _ERROR
      <> ": ❰None❱ is not valid on its own                                      \n\
            \                                                                                \n\
            \Explanation: The conversion to GraphQL does not accept ❰None❱ in isolation as  \n\
            \a valid way to represent ❰null❱.  In Dhall, ❰None❱ is a function whose input is \n\
            \a type and whose output is an ❰Optional❱ of that type.                          \n\
            \                                                                                \n\
            \For example:                                                                    \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐  ❰None❱ is a function whose result is   \n\
            \    │ None : ∀(a : Type) → Optional a │  an ❰Optional❱ value, but the function  \n\
            \    └─────────────────────────────────┘  itself is not a valid ❰Optional❱ value \n\
            \                                                                                \n\
            \                                                                                \n\
            \    ┌─────────────────────────────────┐  ❰None Natural❱ is a valid ❰Optional❱   \n\
            \    │ None Natural : Optional Natural │  value (an absent ❰Natural❱ number in   \n\
            \    └─────────────────────────────────┘  this case)                             \n\
            \                                                                                \n\
            \                                                                                \n\
            \                                                                                \n\
            \The conversion to GraphQL only translates the fully applied form to ❰null❱.   "

  show (Unsupported e) =
    Data.Text.unpack
      $ _ERROR
      <> ": Cannot translate to GraphQL                                         \n\
            \                                                                                \n\
            \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱     \n\
            \values can be translated from Dhall to GraphQL                                  \n\
            \                                                                                \n\
            \The following Dhall expression could not be translated to GraphQL:              \n\
            \                                                                                \n\
            \"
      <> insert e

_ERROR :: Data.Text.Text
_ERROR = Dhall.Util._ERROR

insert :: Pretty a => a -> Text
insert = Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent GraphQL SDL
-}
dhallToGraphQL :: Expr s Void -> Either CompileError Text
dhallToGraphQL e0 = Right _ERROR
