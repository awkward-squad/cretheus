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
    integer,
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
    tuple2,
    tuple3,

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

    -- ** Longer tuple decoders
    tuple4,
    tuple5,
    tuple6,
    tuple7,
    tuple8,
    tuple9,
  )
where

import Cretheus.Internal.Decode
import Prelude hiding (map)
