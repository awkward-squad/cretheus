module Cretheus.Encode
  ( -- * Encoding
    Encoding,
    bytesEncoding,
    valueEncoding,

    -- * Encoders
    bool,
    int,
    text,
    list,
    vector,
    object,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

class Encoding a where
  bool_ :: Bool -> a
  int_ :: Int -> a
  text_ :: Text -> a
  list_ :: (x -> a) -> [x] -> a
  vector_ :: (x -> a) -> Vector x -> a
  object_ :: [(Aeson.Key, a)] -> a

instance Encoding Aeson.Value where
  bool_ = Aeson.toJSON
  int_ = Aeson.toJSON
  text_ = Aeson.toJSON
  list_ f = Aeson.toJSON . map f
  vector_ f = Aeson.Array . Vector.map f
  object_ = Aeson.Object . Aeson.KeyMap.fromList

instance Encoding Aeson.Encoding where
  bool_ = Aeson.bool
  int_ = Aeson.int
  text_ = Aeson.text
  list_ = Aeson.list
  vector_ f = Aeson.list f . Vector.toList
  object_ = Aeson.pairs . foldMap (\(k, v) -> Aeson.pair k v)

-- | Interpret an encoding as bytes.
bytesEncoding :: (forall x. Encoding x => x) -> ByteString
bytesEncoding encoding =
  ByteString.Lazy.toStrict (Aeson.encodingToLazyByteString (encoding :: Aeson.Encoding))

-- | Interpret an encoding as a value.
valueEncoding :: (forall x. Encoding x => x) -> Aeson.Value
valueEncoding encoding =
  encoding

-- | A bool encoder.
bool :: Encoding x => Bool -> x
bool = bool_

-- | An int encoder.
int :: Encoding x => Int -> x
int = int_

-- | A text encoder.
text :: Encoding x => Text -> x
text = text_

-- | A list encoder.
list :: Encoding x => (a -> x) -> [a] -> x
list e = list_ e

-- | A vector encoder.
vector :: Encoding x => (a -> x) -> Vector a -> x
vector e = vector_ e

-- | An object encoder.
object :: Encoding x => [(Aeson.Key, x)] -> x
object = object_
