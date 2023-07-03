module Cretheus.Internal.Decode
  ( -- * Decoder
    Decoder,
    fromBytes,
    fromLazyBytes,
    fromText,

    -- * Decoders
    value,
    bool,
    int64,
    text,
    list,
    Cretheus.Internal.Decode.map,
    refine,

    -- ** Object
    ObjectDecoder,
    property,
    optionalProperty,
    object,
  )
where

import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Aeson.Parser qualified as Aeson (eitherDecodeStrictWith, eitherDecodeWith)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector

newtype GDecoder a b = GDecoder
  { unGDecoder :: a -> Aeson.Parser b
  }
  deriving stock (Functor)

instance Applicative (GDecoder a) where
  pure x = GDecoder \_ -> pure x
  (<*>) = Monad.ap

instance Monad (GDecoder a) where
  return = pure
  GDecoder mx >>= f =
    GDecoder \i ->
      mx i >>= \x ->
        unGDecoder (f x) i

newtype Decoder a
  = Decoder (Aeson.Value -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Value)

newtype ObjectDecoder a
  = ObjectDecoder (Aeson.Object -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Object)

fromBytes :: Decoder a -> ByteString -> Either Text a
fromBytes (Decoder parser) bytes =
  case Aeson.eitherDecodeStrictWith Aeson.json' (Aeson.iparse parser) bytes of
    Left (path, err) -> Left (Text.pack (Aeson.formatError path err))
    Right result -> Right result

fromLazyBytes :: Decoder a -> Lazy.ByteString -> Either Text a
fromLazyBytes (Decoder parser) bytes =
  case Aeson.eitherDecodeWith Aeson.json' (Aeson.iparse parser) bytes of
    Left (path, err) -> Left (Text.pack (Aeson.formatError path err))
    Right result -> Right result

fromText :: Decoder a -> Text -> Either Text a
fromText decoder str =
  fromBytes decoder (Text.encodeUtf8 str)

value :: Decoder Aeson.Value
value =
  Decoder Aeson.parseJSON

bool :: Decoder Bool
bool =
  Decoder Aeson.parseJSON

int64 :: Decoder Int64
int64 =
  Decoder Aeson.parseJSON

text :: Decoder Text
text =
  Decoder Aeson.parseJSON

list :: Decoder v -> Decoder [v]
list (Decoder v) =
  Decoder (Aeson.withArray "" (fmap Vector.toList . traverse v))

map :: Decoder v -> Decoder (Map Text v)
map (Decoder v) =
  object "" (ObjectDecoder (traverse v . Aeson.KeyMap.toMapText))

-- | Refine a decoder with a predicate.
refine :: (a -> Either Text b) -> Decoder a -> Decoder b
refine p (Decoder f) =
  Decoder \val -> do
    x <- f val
    case p x of
      Left err -> fail (Text.unpack err)
      Right y -> pure y

property :: Aeson.Key -> Decoder a -> ObjectDecoder a
property k (Decoder v) =
  ObjectDecoder \o -> Aeson.explicitParseField v o k

optionalProperty :: Aeson.Key -> Decoder a -> ObjectDecoder (Maybe a)
optionalProperty k (Decoder v) =
  ObjectDecoder \o -> Aeson.explicitParseFieldMaybe v o k

object :: String -> ObjectDecoder a -> Decoder a
object name (ObjectDecoder o) =
  Decoder (Aeson.withObject name o)
