module Cretheus.Internal.Encode
  ( Encoding,
    PropertyEncoding,
    asBytes,
    asBytesBuilder,
    asLazyBytes,
    asText,
    asValue,
    bool,
    double,
    float,
    int,
    int32,
    int64,
    list,
    null,
    object,
    optionalProperty,
    property,
    text,
    utcTime,
    vector,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as ByteString (Builder)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (null)

-- | A value encoding.
data Encoding
  = Encoding Aeson.Encoding Aeson.Value

mk :: (a -> Aeson.Encoding) -> (a -> Aeson.Value) -> a -> Encoding
mk f g x =
  Encoding (f x) (g x)

asAesonEncoding :: Encoding -> Aeson.Encoding
asAesonEncoding (Encoding encoding _) =
  encoding

-- | Interpret an encoding as bytes.
asBytes :: Encoding -> ByteString
asBytes =
  ByteString.Lazy.toStrict . asLazyBytes

-- | Interpret an encoding as bytes.
asLazyBytes :: Encoding -> LazyByteString
asLazyBytes =
  Aeson.encodingToLazyByteString . asAesonEncoding

-- | Interpret an encoding as a bytes builder.
asBytesBuilder :: Encoding -> ByteString.Builder
asBytesBuilder =
  Aeson.fromEncoding . asAesonEncoding

-- | Interpret an encoding as text.
asText :: Encoding -> Text
asText =
  Text.decodeUtf8 . asBytes

-- | Interpret an encoding as a value.
asValue :: Encoding -> Aeson.Value
asValue (Encoding _ value) =
  value

-- | A bool encoder.
bool :: Bool -> Encoding
bool =
  mk Aeson.bool Aeson.toJSON

-- | An int encoder.
int :: Int -> Encoding
int =
  int64 . fromIntegral @Int @Int64

-- | A 32-bit int encoder.
int32 :: Int32 -> Encoding
int32 =
  mk Aeson.int32 Aeson.toJSON

-- | A 64-bit int encoder.
int64 :: Int64 -> Encoding
int64 =
  mk Aeson.int64 Aeson.toJSON

-- | A 32-bit float encoder.
float :: Float -> Encoding
float =
  mk Aeson.float Aeson.toJSON

-- | A 32-bit float encoder.
double :: Double -> Encoding
double =
  mk Aeson.double Aeson.toJSON

-- | A text encoder.
text :: Text -> Encoding
text =
  mk Aeson.text Aeson.toJSON

-- | A timestamp encoder (ISO 8601).
utcTime :: UTCTime -> Encoding
utcTime =
  mk Aeson.utcTime Aeson.toJSON

-- | A null encoder.
null :: Encoding
null =
  Encoding Aeson.null_ Aeson.Null

-- | A list encoder.
list :: (a -> Encoding) -> [a] -> Encoding
list f =
  mk toAesonEncoding toAesonValue
  where
    toAesonEncoding = Aeson.list (asAesonEncoding . f)
    toAesonValue = Aeson.toJSON . map (asValue . f)

-- | A vector encoder.
vector :: (a -> Encoding) -> Vector a -> Encoding
vector f =
  mk toAesonEncoding toAesonValue
  where
    toAesonEncoding = Aeson.list (asAesonEncoding . f) . Vector.toList
    toAesonValue = Aeson.Array . Vector.map (asValue . f)

-- | An object property encoding.
data PropertyEncoding
  = Algo !Prop
  | Nada

data Prop
  = Prop !Aeson.Key !Encoding

propToAesonSeries :: Prop -> Aeson.Series
propToAesonSeries (Prop k v) =
  Aeson.pair k (asAesonEncoding v)

propsToAesonEncoding :: [Prop] -> Aeson.Encoding
propsToAesonEncoding =
  Aeson.pairs . foldMap propToAesonSeries

propToAesonKeyValue :: Prop -> (Aeson.Key, Aeson.Value)
propToAesonKeyValue (Prop k v) =
  (k, asValue v)

propsToAesonValue :: [Prop] -> Aeson.Value
propsToAesonValue =
  Aeson.Object . Aeson.KeyMap.fromList . map propToAesonKeyValue

addProperty :: PropertyEncoding -> [Prop] -> [Prop]
addProperty = \case
  Algo prop -> (prop :)
  Nada -> id

-- | An object encoder.
object :: [PropertyEncoding] -> Encoding
object =
  mk propsToAesonEncoding propsToAesonValue . foldr addProperty []

-- | An object property encoder.
property :: Aeson.Key -> Encoding -> PropertyEncoding
property key val =
  Algo (Prop key val)

-- | A optional object property encoder.
optionalProperty :: Aeson.Key -> Maybe Encoding -> PropertyEncoding
optionalProperty key = \case
  Nothing -> Nada
  Just val -> Algo (Prop key val)
