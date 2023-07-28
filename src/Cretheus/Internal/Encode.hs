module Cretheus.Internal.Encode
  ( -- * Encoding
    Encoding,
    SomeEncoding (..),
    asBytes,
    asValue,

    -- * Encoders
    bool,
    int,
    int64,
    text,
    null,
    list,
    vector,
    object,
    property,
    optionalProperty,
    something,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (null)

class Encoding a where
  bool_ :: Bool -> a
  int_ :: Int64 -> a
  text_ :: Text -> a
  list_ :: [a] -> a
  vector_ :: Vector a -> a
  object_ :: [(Aeson.Key, a)] -> a
  null_ :: a

instance Encoding Aeson.Value where
  bool_ = Aeson.toJSON
  int_ = Aeson.toJSON
  text_ = Aeson.toJSON
  list_ = Aeson.toJSON
  vector_ = Aeson.Array
  object_ = Aeson.Object . Aeson.KeyMap.fromList
  null_ = Aeson.Null

instance Encoding Aeson.Encoding where
  bool_ = Aeson.bool
  int_ = Aeson.int64
  text_ = Aeson.text
  list_ = Aeson.list id
  vector_ = Aeson.list id . Vector.toList
  object_ = Aeson.pairs . foldMap (\(k, v) -> Aeson.pair k v)
  null_ = Aeson.null_

data SomeEncoding
  = SomeEncoding (forall a. Encoding a => a)

-- | Interpret an encoding as bytes.
asBytes :: (forall a. Encoding a => a) -> ByteString
asBytes encoding =
  ByteString.Lazy.toStrict (Aeson.encodingToLazyByteString (encoding :: Aeson.Encoding))

-- | Interpret an encoding as a value.
asValue :: (forall a. Encoding a => a) -> Aeson.Value
asValue encoding =
  encoding

-- | A bool encoder.
bool :: Encoding a => Bool -> a
bool = bool_

-- | An int encoder.
int :: Encoding a => Int -> a
int = int_ . fromIntegral @Int @Int64

-- | An int64 encoder.
int64 :: Encoding a => Int64 -> a
int64 = int_

-- | A text encoder.
text :: Encoding a => Text -> a
text = text_

-- | A null encoder.
null :: Encoding a => a
null = null_

-- | A list encoder.
list :: Encoding a => [a] -> a
list = list_

-- | A vector encoder.
vector :: Encoding a => Vector a -> a
vector = vector_

-- | An object encoder.
object :: Encoding a => [Maybe (Aeson.Key, a)] -> a
object = object_ . catMaybes

-- | An object property.
property :: Aeson.Key -> a -> Maybe (Aeson.Key, a)
property key value =
  Just (key, value)

-- | A optional object property.
optionalProperty :: Aeson.Key -> Maybe a -> Maybe (Aeson.Key, a)
optionalProperty key =
  fmap (key,)

-- | A something encoder.
something :: Encoding a => SomeEncoding -> a
something (SomeEncoding x) = x
