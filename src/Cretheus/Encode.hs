module Cretheus.Encode
  ( -- * Encoding
    Encoding,
    asBytes,
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

    -- ** Array encoders
    list,
    listOf,
    vector,
    vectorOf,

    -- ** Object encoders
    PropertyEncoding,
    object,
    property,
    optionalProperty,

    -- ** Null encoders
    null,
  )
where

import Cretheus.Internal.Encode
import Prelude hiding (null)
