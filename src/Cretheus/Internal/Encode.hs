module Cretheus.Internal.Encode
  ( -- * Encoding
    Encoding,
    asBytes,
    asText,
    asValue,

    -- * Basic encoders
    bool,
    int,
    int32,
    int64,
    float32,
    float64,
    text,
    null,
    list,
    vector,

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
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (null)

-- | A value encoding.
data Encoding
  = Encoding Aeson.Encoding Aeson.Value

encode :: (a -> Aeson.Encoding) -> (a -> Aeson.Value) -> a -> Encoding
encode f g x =
  Encoding (f x) (g x)

asAesonEncoding :: Encoding -> Aeson.Encoding
asAesonEncoding (Encoding encoding _) =
  encoding

-- | Interpret an encoding as bytes.
asBytes :: Encoding -> ByteString
asBytes =
  ByteString.Lazy.toStrict . Aeson.encodingToLazyByteString . asAesonEncoding

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
  encode Aeson.bool Aeson.toJSON

-- | An int encoder.
int :: Int -> Encoding
int =
  int64 . fromIntegral @Int @Int64

-- | A 32-bit int encoder.
int32 :: Int32 -> Encoding
int32 =
  encode Aeson.int32 Aeson.toJSON

-- | A 64-bit int encoder.
int64 :: Int64 -> Encoding
int64 =
  encode Aeson.int64 Aeson.toJSON

-- | A 32-bit float encoder.
float32 :: Float -> Encoding
float32 =
  encode Aeson.float Aeson.toJSON

-- | A 32-bit float encoder.
float64 :: Double -> Encoding
float64 =
  encode Aeson.double Aeson.toJSON

-- | A text encoder.
text :: Text -> Encoding
text =
  encode Aeson.text Aeson.toJSON

-- | A null encoder.
null :: Encoding
null =
  Encoding Aeson.null_ Aeson.Null

-- | A list encoder.
list :: [Encoding] -> Encoding
list =
  encode (Aeson.list id . map asAesonEncoding) (Aeson.toJSON . map asValue)

-- | A vector encoder.
vector :: Vector Encoding -> Encoding
vector =
  encode (Aeson.list id . Vector.toList . Vector.map asAesonEncoding) (Aeson.Array . Vector.map asValue)

-- | An object property encoding.
data PropertyEncoding
  = Algo !Prop
  | Nada

data Prop
  = Prop !Aeson.Key !Encoding

addProperty :: PropertyEncoding -> [Prop] -> [Prop]
addProperty = \case
  Algo prop -> (prop :)
  Nada -> id

-- | An object encoder.
object :: [PropertyEncoding] -> Encoding
object =
  encode toEncoding toValue . foldr addProperty []
  where
    toEncoding :: [Prop] -> Aeson.Encoding
    toEncoding =
      Aeson.pairs . foldMap (\(Prop k v) -> Aeson.pair k (asAesonEncoding v))

    toValue :: [Prop] -> Aeson.Value
    toValue =
      Aeson.Object . Aeson.KeyMap.fromList . map (\(Prop k v) -> (k, asValue v))

-- | An object property encoder.
property :: Aeson.Key -> Encoding -> PropertyEncoding
property key val =
  Algo (Prop key val)

-- | A optional object property encoder.
optionalProperty :: Aeson.Key -> Maybe Encoding -> PropertyEncoding
optionalProperty key = \case
  Nothing -> Nada
  Just val -> Algo (Prop key val)
