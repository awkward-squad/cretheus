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
  )
where

import Cretheus.Internal.Encode
import Prelude hiding (map, null)
