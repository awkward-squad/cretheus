module Cretheus.Internal.Encode
  ( Encoding,
    PropertyEncoding,
    array,
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
    keyMap,
    list,
    map,
    null,
    object,
    optionalProperty,
    property,
    set,
    text,
    tuple2,
    tuple3,
    tuple4,
    tuple5,
    tuple6,
    tuple7,
    tuple8,
    tuple9,
    utcTime,
    value,
    vector,
  )
where

import Control.Monad.ST (runST)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.Encoding.Internal qualified as Aeson (Encoding' (..), closeBracket, comma, openBracket)
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson (KeyMap)
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as ByteString (Builder)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Foldable qualified as Foldable
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (Array)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Vector.Mutable
import Prelude hiding (map, null)

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
{-# INLINEABLE asBytes #-}

-- | Interpret an encoding as bytes.
asLazyBytes :: Encoding -> LazyByteString
asLazyBytes =
  Aeson.encodingToLazyByteString . asAesonEncoding
{-# INLINEABLE asLazyBytes #-}

-- | Interpret an encoding as a bytes builder.
asBytesBuilder :: Encoding -> ByteString.Builder
asBytesBuilder =
  Aeson.fromEncoding . asAesonEncoding
{-# INLINEABLE asBytesBuilder #-}

-- | Interpret an encoding as text.
asText :: Encoding -> Text
asText =
  Text.decodeUtf8 . asBytes
{-# INLINEABLE asText #-}

-- | Interpret an encoding as a value.
asValue :: Encoding -> Aeson.Value
asValue (Encoding _ val) =
  val
{-# INLINEABLE asValue #-}

-- | A value encoder.
value :: Aeson.Value -> Encoding
value val =
  Encoding (Aeson.value val) val
{-# INLINEABLE value #-}

-- | A bool encoder.
bool :: Bool -> Encoding
bool =
  mk Aeson.bool Aeson.toJSON
{-# INLINEABLE bool #-}

-- | An int encoder.
int :: Int -> Encoding
int =
  int64 . fromIntegral @Int @Int64
{-# INLINEABLE int #-}

-- | A 32-bit int encoder.
int32 :: Int32 -> Encoding
int32 =
  mk Aeson.int32 Aeson.toJSON
{-# INLINEABLE int32 #-}

-- | A 64-bit int encoder.
int64 :: Int64 -> Encoding
int64 =
  mk Aeson.int64 Aeson.toJSON
{-# INLINEABLE int64 #-}

-- | A 32-bit float encoder.
float :: Float -> Encoding
float =
  mk Aeson.float Aeson.toJSON
{-# INLINEABLE float #-}

-- | A 32-bit float encoder.
double :: Double -> Encoding
double =
  mk Aeson.double Aeson.toJSON
{-# INLINEABLE double #-}

-- | A text encoder.
text :: Text -> Encoding
text =
  mk Aeson.text Aeson.toJSON
{-# INLINEABLE text #-}

-- | A timestamp encoder (ISO 8601).
utcTime :: UTCTime -> Encoding
utcTime =
  mk Aeson.utcTime Aeson.toJSON
{-# INLINEABLE utcTime #-}

-- | A null encoder.
null :: Encoding
null =
  Encoding Aeson.null_ Aeson.Null
{-# INLINEABLE null #-}

-- | A list encoder.
list :: (a -> Encoding) -> [a] -> Encoding
list f =
  mk (Aeson.list (asAesonEncoding . f)) (Aeson.toJSON . List.map (asValue . f))
{-# INLINEABLE list #-}

-- | An array encoder.
array :: (a -> Encoding) -> Array a -> Encoding
array f =
  mk (Aeson.list (asAesonEncoding . f) . Foldable.toList @Array) (Aeson.Array . Vector.fromArray . fmap @Array (asValue . f))
{-# INLINEABLE array #-}

-- | A vector encoder.
vector :: (a -> Encoding) -> Vector a -> Encoding
vector f =
  mk (Aeson.list (asAesonEncoding . f) . Vector.toList) (Aeson.Array . Vector.map (asValue . f))
{-# INLINEABLE vector #-}

-- | A set encoder.
set :: (a -> Encoding) -> Set a -> Encoding
set f =
  list f . Set.toList
{-# INLINEABLE set #-}

-- | A 2-tuple encoder.
tuple2 :: (a -> Encoding) -> (b -> Encoding) -> (a, b) -> Encoding
tuple2 fa fb =
  mk
    ( \(a, b) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< Aeson.comma
          >< asAesonEncoding (fb b)
          >< Aeson.closeBracket
    )
    ( \(a, b) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 2
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple2 #-}

-- | A 3-tuple encoder.
tuple3 :: (a -> Encoding) -> (b -> Encoding) -> (c -> Encoding) -> (a, b, c) -> Encoding
tuple3 fa fb fc =
  mk
    ( \(a, b, c) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr (\x acc -> Aeson.comma >< asAesonEncoding x >< acc) Aeson.closeBracket [fb b, fc c]
    )
    ( \(a, b, c) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 3
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple3 #-}

-- | A 4-tuple encoder.
tuple4 :: (a -> Encoding) -> (b -> Encoding) -> (c -> Encoding) -> (d -> Encoding) -> (a, b, c, d) -> Encoding
tuple4 fa fb fc fd =
  mk
    ( \(a, b, c, d) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr (\x acc -> Aeson.comma >< asAesonEncoding x >< acc) Aeson.closeBracket [fb b, fc c, fd d]
    )
    ( \(a, b, c, d) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 4
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.Mutable.write v 3 (asValue (fd d))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple4 #-}

-- | A 5-tuple encoder.
tuple5 ::
  (a -> Encoding) ->
  (b -> Encoding) ->
  (c -> Encoding) ->
  (d -> Encoding) ->
  (e -> Encoding) ->
  (a, b, c, d, e) ->
  Encoding
tuple5 fa fb fc fd fe =
  mk
    ( \(a, b, c, d, e) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr (\x acc -> Aeson.comma >< asAesonEncoding x >< acc) Aeson.closeBracket [fb b, fc c, fd d, fe e]
    )
    ( \(a, b, c, d, e) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 5
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.Mutable.write v 3 (asValue (fd d))
              Vector.Mutable.write v 4 (asValue (fe e))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple5 #-}

-- | A 6-tuple encoder.
tuple6 ::
  (a -> Encoding) ->
  (b -> Encoding) ->
  (c -> Encoding) ->
  (d -> Encoding) ->
  (e -> Encoding) ->
  (f -> Encoding) ->
  (a, b, c, d, e, f) ->
  Encoding
tuple6 fa fb fc fd fe ff =
  mk
    ( \(a, b, c, d, e, f) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr (\x acc -> Aeson.comma >< asAesonEncoding x >< acc) Aeson.closeBracket [fb b, fc c, fd d, fe e, ff f]
    )
    ( \(a, b, c, d, e, f) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 6
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.Mutable.write v 3 (asValue (fd d))
              Vector.Mutable.write v 4 (asValue (fe e))
              Vector.Mutable.write v 5 (asValue (ff f))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple6 #-}

-- | A 7-tuple encoder.
tuple7 ::
  (a -> Encoding) ->
  (b -> Encoding) ->
  (c -> Encoding) ->
  (d -> Encoding) ->
  (e -> Encoding) ->
  (f -> Encoding) ->
  (g -> Encoding) ->
  (a, b, c, d, e, f, g) ->
  Encoding
tuple7 fa fb fc fd fe ff fg =
  mk
    ( \(a, b, c, d, e, f, g) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr
            (\x acc -> Aeson.comma >< asAesonEncoding x >< acc)
            Aeson.closeBracket
            [ fb b,
              fc c,
              fd d,
              fe e,
              ff f,
              fg g
            ]
    )
    ( \(a, b, c, d, e, f, g) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 7
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.Mutable.write v 3 (asValue (fd d))
              Vector.Mutable.write v 4 (asValue (fe e))
              Vector.Mutable.write v 5 (asValue (ff f))
              Vector.Mutable.write v 6 (asValue (fg g))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple7 #-}

-- | A 8-tuple encoder.
tuple8 ::
  (a -> Encoding) ->
  (b -> Encoding) ->
  (c -> Encoding) ->
  (d -> Encoding) ->
  (e -> Encoding) ->
  (f -> Encoding) ->
  (g -> Encoding) ->
  (h -> Encoding) ->
  (a, b, c, d, e, f, g, h) ->
  Encoding
tuple8 fa fb fc fd fe ff fg fh =
  mk
    ( \(a, b, c, d, e, f, g, h) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr
            (\x acc -> Aeson.comma >< asAesonEncoding x >< acc)
            Aeson.closeBracket
            [ fb b,
              fc c,
              fd d,
              fe e,
              ff f,
              fg g,
              fh h
            ]
    )
    ( \(a, b, c, d, e, f, g, h) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 8
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.Mutable.write v 3 (asValue (fd d))
              Vector.Mutable.write v 4 (asValue (fe e))
              Vector.Mutable.write v 5 (asValue (ff f))
              Vector.Mutable.write v 6 (asValue (fg g))
              Vector.Mutable.write v 7 (asValue (fh h))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple8 #-}

-- | A 9-tuple encoder.
tuple9 ::
  (a -> Encoding) ->
  (b -> Encoding) ->
  (c -> Encoding) ->
  (d -> Encoding) ->
  (e -> Encoding) ->
  (f -> Encoding) ->
  (g -> Encoding) ->
  (h -> Encoding) ->
  (i -> Encoding) ->
  (a, b, c, d, e, f, g, h, i) ->
  Encoding
tuple9 fa fb fc fd fe ff fg fh fi =
  mk
    ( \(a, b, c, d, e, f, g, h, i) ->
        Aeson.openBracket
          >< asAesonEncoding (fa a)
          >< foldr
            (\x acc -> Aeson.comma >< asAesonEncoding x >< acc)
            Aeson.closeBracket
            [ fb b,
              fc c,
              fd d,
              fe e,
              ff f,
              fg g,
              fh h,
              fi i
            ]
    )
    ( \(a, b, c, d, e, f, g, h, i) ->
        Aeson.Array
          ( runST do
              v <- Vector.Mutable.unsafeNew 9
              Vector.Mutable.write v 0 (asValue (fa a))
              Vector.Mutable.write v 1 (asValue (fb b))
              Vector.Mutable.write v 2 (asValue (fc c))
              Vector.Mutable.write v 3 (asValue (fd d))
              Vector.Mutable.write v 4 (asValue (fe e))
              Vector.Mutable.write v 5 (asValue (ff f))
              Vector.Mutable.write v 6 (asValue (fg g))
              Vector.Mutable.write v 7 (asValue (fh h))
              Vector.Mutable.write v 8 (asValue (fi i))
              Vector.unsafeFreeze v
          )
    )
{-# INLINEABLE tuple9 #-}

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
  Aeson.Object . Aeson.KeyMap.fromList . List.map propToAesonKeyValue

addProperty :: PropertyEncoding -> [Prop] -> [Prop]
addProperty = \case
  Algo prop -> (prop :)
  Nada -> id

-- | An object encoder.
object :: [PropertyEncoding] -> Encoding
object =
  mk propsToAesonEncoding propsToAesonValue . foldr addProperty []
{-# INLINEABLE object #-}

-- | An object property encoder.
property :: Aeson.Key -> Encoding -> PropertyEncoding
property key val =
  Algo (Prop key val)
{-# INLINEABLE property #-}

-- | A optional object property encoder.
optionalProperty :: Aeson.Key -> Maybe Encoding -> PropertyEncoding
optionalProperty key = \case
  Nothing -> Nada
  Just val -> Algo (Prop key val)
{-# INLINEABLE optionalProperty #-}

-- | A map encoder.
map :: (k -> Aeson.Key) -> (a -> Encoding) -> Map k a -> Encoding
map f g =
  mk
    (Aeson.dict (Aeson.text . Aeson.Key.toText . f) (asAesonEncoding . g) Map.foldrWithKey)
    (Aeson.Object . Aeson.KeyMap.fromList . List.map (\(k, v) -> (f k, asValue (g v))) . Map.toList)
{-# INLINEABLE map #-}

-- | A key map encoder.
keyMap :: (a -> Encoding) -> Aeson.KeyMap a -> Encoding
keyMap f =
  mk
    (Aeson.dict (Aeson.text . Aeson.Key.toText) (asAesonEncoding . f) Aeson.KeyMap.foldrWithKey)
    (Aeson.Object . Aeson.KeyMap.map (asValue . f))
{-# INLINEABLE keyMap #-}

-- Copied from aeson source because they don't provide a Semigroup on Encoding...

infixr 6 ><

(><) :: Aeson.Encoding' a -> Aeson.Encoding' a -> Aeson.Encoding' a
Aeson.Encoding a >< Aeson.Encoding b = Aeson.Encoding (a <> b)
