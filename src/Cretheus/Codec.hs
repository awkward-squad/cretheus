module Cretheus.Codec
  ( -- * Codec
    Codec (..),

    -- ** Boolean codecs
    bool,

    -- ** Number codecs
    int,
    int32,
    int64,
    float,
    double,

    -- ** String codecs
    text,
    utcTime,

    -- ** Array codecs
    list,
    array,
    vector,
    set,

    -- ** Object codecs
    ObjectCodec (..),
    object,
    map,
    keyMap,

    -- ** Null codecs
    null,
  )
where

import Cretheus.Internal.Decode (Decoder, ObjectDecoder)
import Cretheus.Internal.Decode qualified as Decode
import Cretheus.Internal.Encode (Encoding, PropertyEncoding)
import Cretheus.Internal.Encode qualified as Encode
import Data.Aeson.KeyMap qualified as Aeson (Key, KeyMap)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Primitive.Array (Array)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Prelude hiding (map, null)

-- | A value codec.
data Codec a = Codec
  { decoder :: Decoder a,
    encoder :: a -> Encoding
  }

-- | An object codec.
data ObjectCodec a = ObjectCodec
  { decoder :: ObjectDecoder a,
    encoder :: a -> [PropertyEncoding]
  }

-- | A bool codec.
bool :: Codec Bool
bool =
  Codec Decode.bool Encode.bool

-- | An int codec.
int :: Codec Int
int =
  Codec Decode.int Encode.int

-- | A 32-bit int codec.
int32 :: Codec Int32
int32 =
  Codec Decode.int32 Encode.int32

-- | A 64-bit int codec.
int64 :: Codec Int64
int64 =
  Codec Decode.int64 Encode.int64

-- | A 32-bit float codec.
float :: Codec Float
float =
  Codec Decode.float Encode.float

-- | A 64-bit float codec.
double :: Codec Double
double =
  Codec Decode.double Encode.double

-- | A text codec.
text :: Codec Text
text =
  Codec Decode.text Encode.text

-- | A timestamp codec (ISO 8601).
utcTime :: Codec UTCTime
utcTime =
  Codec Decode.utcTime Encode.utcTime

-- | A list codec.
list :: Codec a -> Codec [a]
list codec =
  Codec (Decode.list codec.decoder) (Encode.list codec.encoder)

-- | An array codec.
array :: Codec a -> Codec (Array a)
array codec =
  Codec (Decode.array codec.decoder) (Encode.array codec.encoder)

-- | A vector codec.
vector :: Codec a -> Codec (Vector a)
vector codec =
  Codec (Decode.vector codec.decoder) (Encode.vector codec.encoder)

-- | A set codec.
set :: (Ord a) => Codec a -> Codec (Set a)
set codec =
  Codec (Decode.set codec.decoder) (Encode.set codec.encoder)

-- | An object codec.
object :: ObjectCodec a -> Codec a
object codec =
  Codec (Decode.object codec.decoder) (Encode.object . codec.encoder)

-- | A map codec.
map :: (Ord k) => (Aeson.Key -> k) -> (k -> Aeson.Key) -> Codec a -> Codec (Map k a)
map fromKey toKey codec =
  Codec (Decode.map fromKey codec.decoder) (Encode.map toKey codec.encoder)

-- | A key map codec.
keyMap :: Codec a -> Codec (Aeson.KeyMap a)
keyMap codec =
  Codec (Decode.keyMap codec.decoder) (Encode.keyMap codec.encoder)

-- | A null codec.
null :: Codec ()
null =
  Codec Decode.null (const Encode.null)
