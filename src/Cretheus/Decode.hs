module Cretheus.Decode
  ( -- * Decoding
    Decoder (..),
    fromBytes,
    fromLazyBytes,
    fromText,
    fromValue,

    -- * Decoders

    -- ** Boolean decoders
    bool,

    -- ** Number decoders
    int,
    int32,
    int64,
    float,
    double,

    -- ** String decoders
    text,

    -- ** Array decoders
    list,
    vector,

    -- ** Null decoders
    nullable,

    -- ** Object decoders
    ObjectDecoder (..),
    object,
    property,
    optionalProperty,
    map,

    -- ** Value decoders
    value,

    -- ** Decoder refinement
    refine,
  )
where

import Cretheus.Internal.Decode
import Prelude hiding (map)
