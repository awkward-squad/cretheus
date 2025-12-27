module Cretheus.Encode
  ( -- * Encoding
    Encoding,
    asBytes,
    asLazyBytes,
    asBytesBuilder,
    asText,
    asValue,

    -- * Encoders

    -- ** Boolean encoders
    bool,

    -- ** Number encoders
    int,
    int32,
    int64,
    float,
    double,

    -- ** String encoders
    text,
    utcTime,

    -- ** Array encoders
    list,
    array,
    vector,
    set,
    tuple2,
    tuple3,

    -- ** Object encoders
    PropertyEncoding,
    object,
    property,
    optionalProperty,
    map,
    keyMap,

    -- ** Null encoders
    null,

    -- ** Value encoders
    value,

    -- ** Longer tuple encoders
    tuple4,
    tuple5,
    tuple6,
    tuple7,
    tuple8,
    tuple9,
  )
where

import Cretheus.Internal.Encode
import Prelude hiding (map, null)
