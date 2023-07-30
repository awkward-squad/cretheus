module Cretheus.Internal.Encode
  ( -- * Encoding
    Encoding,
    SomeEncoding (..),
    asBytes,
    asText,
    asValue,

    -- * Basic encoders
    bool,
    int,
    int64,
    text,
    null,
    list,
    vector,
    something,

    -- ** Object encoders
    PropertyEncoding,
    object,
    property,
    optionalProperty,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (null)

-- | An encoding, which can be interpreted as a structured value or unstructured text.
class Encoding a where
  bool_ :: Bool -> a
  bool_ = undefined
  int_ :: Int64 -> a
  int_ = undefined
  text_ :: Text -> a
  text_ = undefined
  list_ :: [a] -> a
  list_ = undefined
  vector_ :: Vector a -> a
  vector_ = undefined
  object_ :: [Prop a] -> a
  object_ = undefined
  null_ :: a
  null_ = undefined

instance Encoding Aeson.Value where
  bool_ = Aeson.toJSON
  int_ = Aeson.toJSON
  text_ = Aeson.toJSON
  list_ = Aeson.toJSON
  vector_ = Aeson.Array
  object_ = Aeson.Object . Aeson.KeyMap.fromList . map (\(Prop k v) -> (k, v))
  null_ = Aeson.Null

instance Encoding Aeson.Encoding where
  bool_ = Aeson.bool
  int_ = Aeson.int64
  text_ = Aeson.text
  list_ = Aeson.list id
  vector_ = Aeson.list id . Vector.toList
  object_ = Aeson.pairs . foldMap (\(Prop k v) -> Aeson.pair k v)
  null_ = Aeson.null_

data SomeEncoding
  = SomeEncoding (forall a. Encoding a => a)

-- | Interpret an encoding as bytes.
asBytes :: (forall a. Encoding a => a) -> ByteString
asBytes encoding =
  ByteString.Lazy.toStrict (Aeson.encodingToLazyByteString (encoding :: Aeson.Encoding))

-- | Interpret an encoding as text.
asText :: (forall a. Encoding a => a) -> Text
asText encoding =
  Text.decodeUtf8 (asBytes encoding)

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

-- | A something encoder.
something :: Encoding a => SomeEncoding -> a
something (SomeEncoding x) = x

-- | An object property encoding.
data PropertyEncoding a
  = Algo !(Prop a)
  | Nada

data Prop a
  = Prop !Aeson.Key !a

-- | An object encoder.
object :: Encoding a => [PropertyEncoding a] -> a
object = object_ . props
  where
    props :: [PropertyEncoding a] -> [Prop a]
    props =
      foldr
        ( \case
            Algo prop -> (prop :)
            Nada -> id
        )
        []

-- | An object property encoder.
property :: Aeson.Key -> a -> PropertyEncoding a
property key val =
  Algo (Prop key val)

-- | A optional object property encoder.
optionalProperty :: Aeson.Key -> Maybe a -> PropertyEncoding a
optionalProperty key = \case
  Nothing -> Nada
  Just val -> Algo (Prop key val)
