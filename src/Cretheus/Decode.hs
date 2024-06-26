module Cretheus.Decode
  ( -- * Decoding
    Decoder,
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
    utcTime,

    -- ** Array decoders
    list,
    array,
    vector,
    set,

    -- ** Object decoders
    ObjectDecoder,
    object,
    property,
    optionalProperty,
    map,
    keyMap,

    -- ** Null decoders
    nullable,

    -- ** Value decoders
    value,

    -- ** Decoder refinement
    refine,
  )
where

import Cretheus.Internal.Decode
import Prelude hiding (map)
