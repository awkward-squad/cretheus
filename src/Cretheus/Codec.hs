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
    vector,

    -- ** Object codecs
    ObjectCodec (..),
    object,

    -- ** Null codecs
    null,
  )
where

import Cretheus.Internal.Decode (Decoder, ObjectDecoder)
import Cretheus.Internal.Decode qualified as Decode
import Cretheus.Internal.Encode (Encoding, PropertyEncoding)
import Cretheus.Internal.Encode qualified as Encode
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Prelude hiding (null)

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
  Codec (Decode.list codec.decoder) (Encode.listOf codec.encoder)

-- | A vector codec.
vector :: Codec a -> Codec (Vector a)
vector codec =
  Codec (Decode.vector codec.decoder) (Encode.vectorOf codec.encoder)

-- | An object codec.
object :: ObjectCodec a -> Codec a
object codec =
  Codec (Decode.object codec.decoder) (Encode.object . codec.encoder)

-- | A null codec.
null :: Codec ()
null =
  Codec Decode.null (const Encode.null)
