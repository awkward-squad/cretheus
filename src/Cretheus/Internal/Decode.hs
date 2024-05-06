module Cretheus.Internal.Decode
  ( Decoder,
    ObjectDecoder,
    bool,
    fromBytes,
    fromLazyBytes,
    fromText,
    fromValue,
    int,
    int32,
    int64,
    float,
    double,
    list,
    map,
    null,
    nullable,
    object,
    optionalProperty,
    property,
    refine,
    text,
    utcTime,
    value,
    vector,
  )
where

import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (KeyMap)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Int (Int32, Int64)
import Data.Reflection (Reifies (reflect), reify)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (map, null)

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

-- | A value decoder.
newtype Decoder a
  = Decoder (Aeson.Value -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Value)

-- | An object decoder.
newtype ObjectDecoder a
  = ObjectDecoder (Aeson.Object -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Object)

-- This didn't suck as bad before aeson-2.2, when they got rid of `eitherDecodeWith`...
newtype D s a = D a

instance (Reifies s (Decoder a)) => Aeson.FromJSON (D s a) where
  parseJSON :: Aeson.Value -> Aeson.Parser (D s a)
  parseJSON =
    coerce
      @(Decoder a)
      @(Aeson.Value -> Aeson.Parser (D s a))
      (reflect (Proxy :: Proxy s))

-- | Decode bytes.
fromBytes :: Decoder a -> ByteString -> Either Text a
fromBytes decoder bytes =
  reify decoder \(_ :: Proxy s) ->
    case Aeson.eitherDecodeStrict bytes of
      Left err -> Left (Text.pack err)
      Right (D result :: D s a) -> Right result

-- | Decode lazy bytes.
fromLazyBytes :: Decoder a -> Lazy.ByteString -> Either Text a
fromLazyBytes decoder bytes =
  reify decoder \(_ :: Proxy s) ->
    case Aeson.eitherDecode bytes of
      Left err -> Left (Text.pack err)
      Right (D result :: D s a) -> Right result

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

-- | An int decoder.
int :: Decoder Int
int =
  Decoder Aeson.parseJSON

-- | A 32-bit int decoder.
int32 :: Decoder Int32
int32 =
  Decoder Aeson.parseJSON

-- | A 64-bit int decoder.
int64 :: Decoder Int64
int64 =
  Decoder Aeson.parseJSON

-- | A 32-bit float decoder.
float :: Decoder Float
float =
  Decoder Aeson.parseJSON

-- | A 64-bit float decoder.
double :: Decoder Double
double =
  Decoder Aeson.parseJSON

-- | A text decoder.
text :: Decoder Text
text =
  Decoder Aeson.parseJSON

-- | A timestamp decoder (ISO 8601).
utcTime :: Decoder UTCTime
utcTime =
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
map :: Decoder v -> Decoder (Aeson.KeyMap v)
map (Decoder v) =
  object (ObjectDecoder (traverse v))

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

-- | A null decoder.
null :: Decoder ()
null =
  Decoder \case
    Aeson.Null -> pure ()
    _ -> fail "expected null"

-- | A nullable decoder.
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
