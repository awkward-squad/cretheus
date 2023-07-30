module Cretheus.Internal.Decode
  ( -- * Decoder
    Decoder,
    fromBytes,
    fromLazyBytes,
    fromText,
    fromValue,

    -- * Decoders
    value,
    bool,
    int64,
    text,
    vector,
    list,
    Cretheus.Internal.Decode.map,
    nullable,
    refine,

    -- ** Object decoders
    ObjectDecoder,
    object,
    property,
    optionalProperty,
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
import Data.Vector (Vector)
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

-- | A decoder.
newtype Decoder a
  = Decoder (Aeson.Value -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Value)

-- | An object decoder.
newtype ObjectDecoder a
  = ObjectDecoder (Aeson.Object -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Object)

-- | Decode bytes.
fromBytes :: Decoder a -> ByteString -> Either Text a
fromBytes (Decoder parser) bytes =
  case Aeson.eitherDecodeStrictWith Aeson.json' (Aeson.iparse parser) bytes of
    Left (path, err) -> Left (Text.pack (Aeson.formatError path err))
    Right result -> Right result

-- | Decode lazy bytes.
fromLazyBytes :: Decoder a -> Lazy.ByteString -> Either Text a
fromLazyBytes (Decoder parser) bytes =
  case Aeson.eitherDecodeWith Aeson.json' (Aeson.iparse parser) bytes of
    Left (path, err) -> Left (Text.pack (Aeson.formatError path err))
    Right result -> Right result

-- | Decode text.
fromText :: Decoder a -> Text -> Either Text a
fromText decoder str =
  fromBytes decoder (Text.encodeUtf8 str)

-- | Decode a value.
fromValue :: Decoder a -> Aeson.Value -> Either Text a
fromValue (Decoder decoder) val =
  case Aeson.parseEither decoder val of
    Left err -> Left (Text.pack err)
    Right result -> Right result

-- | A value decoder.
value :: Decoder Aeson.Value
value =
  Decoder Aeson.parseJSON

-- | A bool decoder.
bool :: Decoder Bool
bool =
  Decoder Aeson.parseJSON

-- | An int64 decoder.
int64 :: Decoder Int64
int64 =
  Decoder Aeson.parseJSON

-- | A text decoder.
text :: Decoder Text
text =
  Decoder Aeson.parseJSON

-- | A vector decoder.
vector :: Decoder v -> Decoder (Vector v)
vector (Decoder f) =
  Decoder (Aeson.withArray "" (traverse f))

-- | A list decoder.
list :: Decoder v -> Decoder [v]
list =
  fmap Vector.toList . vector

-- | A map decoder.
map :: Decoder v -> Decoder (Map Text v)
map (Decoder v) =
  object (ObjectDecoder (traverse v . Aeson.KeyMap.toMapText))

-- | Modify a decoder to also accept a @null@ value.
nullable :: Decoder v -> Decoder (Maybe v)
nullable (Decoder f) =
  Decoder \case
    Aeson.Null -> pure Nothing
    val -> Just <$> f val

-- | Refine a decoder with a predicate.
refine :: (a -> Either Text b) -> Decoder a -> Decoder b
refine p (Decoder f) =
  Decoder \val -> do
    x <- f val
    case p x of
      Left err -> fail (Text.unpack err)
      Right y -> pure y

-- | An object property decoder.
property :: Aeson.Key -> Decoder a -> ObjectDecoder a
property k (Decoder v) =
  ObjectDecoder \o -> Aeson.explicitParseField v o k

-- | An optional object property decoder.
optionalProperty :: Aeson.Key -> Decoder a -> ObjectDecoder (Maybe a)
optionalProperty k (Decoder v) =
  ObjectDecoder \o -> Aeson.explicitParseFieldMaybe' v o k

-- | An object decoder.
object :: ObjectDecoder a -> Decoder a
object (ObjectDecoder o) =
  Decoder (Aeson.withObject "" o)
