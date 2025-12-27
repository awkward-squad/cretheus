module Cretheus.Internal.Decode
  ( Decoder,
    ObjectDecoder,
    array,
    bool,
    double,
    float,
    fromBytes,
    fromLazyBytes,
    fromText,
    fromValue,
    int,
    int32,
    int64,
    integer,
    keyMap,
    list,
    map,
    null,
    nullable,
    object,
    optionalProperty,
    property,
    refine,
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

import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson (KeyMap)
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (Array)
import Data.Reflection (Reifies (reflect), reify)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (map, null)

newtype GDecoder a b = GDecoder
  { unGDecoder :: a -> Aeson.Parser b
  }
  deriving stock (Functor)

instance Applicative (GDecoder a) where
  pure x = GDecoder \_ -> pure x
  (<*>) = Monad.ap

instance Monad (GDecoder a) where
  return = pure
  GDecoder mx >>= f =
    GDecoder \i ->
      mx i >>= \x ->
        unGDecoder (f x) i

-- | A value decoder.
newtype Decoder a
  = Decoder (Aeson.Value -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Value)

-- | An object decoder.
newtype ObjectDecoder a
  = ObjectDecoder (Aeson.Object -> Aeson.Parser a)
  deriving (Applicative, Functor, Monad) via (GDecoder Aeson.Object)

-- This didn't suck as bad before aeson-2.2, when they got rid of `eitherDecodeWith`...
newtype D (s :: k) a = D a

instance (Reifies s (Decoder a)) => Aeson.FromJSON (D s a) where
  parseJSON :: Aeson.Value -> Aeson.Parser (D s a)
  parseJSON =
    coerce
      @(Decoder a)
      @(Aeson.Value -> Aeson.Parser (D s a))
      (reflect (Proxy :: Proxy s))

-- | Decode bytes.
fromBytes :: Decoder a -> ByteString -> Either Text a
fromBytes decoder bytes =
  reify decoder \(_ :: Proxy s) ->
    case Aeson.eitherDecodeStrict bytes of
      Left err -> Left (Text.pack err)
      Right (D result :: D s a) -> Right result
{-# INLINEABLE fromBytes #-}

-- | Decode lazy bytes.
fromLazyBytes :: Decoder a -> Lazy.ByteString -> Either Text a
fromLazyBytes decoder bytes =
  reify decoder \(_ :: Proxy s) ->
    case Aeson.eitherDecode bytes of
      Left err -> Left (Text.pack err)
      Right (D result :: D s a) -> Right result
{-# INLINEABLE fromLazyBytes #-}

-- | Decode text.
fromText :: Decoder a -> Text -> Either Text a
fromText decoder str =
  fromBytes decoder (Text.encodeUtf8 str)
{-# INLINEABLE fromText #-}

-- | Decode a value.
fromValue :: Decoder a -> Aeson.Value -> Either Text a
fromValue (Decoder decoder) val =
  case Aeson.parseEither decoder val of
    Left err -> Left (Text.pack err)
    Right result -> Right result
{-# INLINEABLE fromValue #-}

-- | A value decoder.
value :: Decoder Aeson.Value
value =
  Decoder Aeson.parseJSON
{-# INLINEABLE value #-}

-- | A bool decoder.
bool :: Decoder Bool
bool =
  Decoder Aeson.parseJSON
{-# INLINEABLE bool #-}

-- | An int decoder.
int :: Decoder Int
int =
  Decoder Aeson.parseJSON
{-# INLINEABLE int #-}

-- | A 32-bit int decoder.
int32 :: Decoder Int32
int32 =
  Decoder Aeson.parseJSON
{-# INLINEABLE int32 #-}

-- | A 64-bit int decoder.
int64 :: Decoder Int64
int64 =
  Decoder Aeson.parseJSON
{-# INLINEABLE int64 #-}

-- | An integer decoder.
integer :: Decoder Integer
integer =
  Decoder Aeson.parseJSON
{-# INLINEABLE integer #-}

-- | A 32-bit float decoder.
float :: Decoder Float
float =
  Decoder Aeson.parseJSON
{-# INLINEABLE float #-}

-- | A 64-bit float decoder.
double :: Decoder Double
double =
  Decoder Aeson.parseJSON
{-# INLINEABLE double #-}

-- | A text decoder.
text :: Decoder Text
text =
  Decoder Aeson.parseJSON
{-# INLINEABLE text #-}

-- | A timestamp decoder (ISO 8601).
utcTime :: Decoder UTCTime
utcTime =
  Decoder Aeson.parseJSON
{-# INLINEABLE utcTime #-}

-- | A list decoder.
list :: Decoder a -> Decoder [a]
list =
  fmap Vector.toList . vector
{-# INLINEABLE list #-}

-- | An array decoder.
array :: Decoder a -> Decoder (Array a)
array (Decoder f) =
  Decoder (Aeson.withArray "" (traverse f . Vector.toArray))
{-# INLINEABLE array #-}

-- | A vector decoder.
vector :: Decoder a -> Decoder (Vector a)
vector (Decoder f) =
  Decoder (Aeson.withArray "" (traverse f))
{-# INLINEABLE vector #-}

-- | A set decoder.
set :: (Ord a) => Decoder a -> Decoder (Set a)
set =
  fmap Set.fromList . list
{-# INLINEABLE set #-}

-- | A 2-tuple decoder.
tuple2 :: (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
tuple2 f (Decoder fa) (Decoder fb) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 2
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
          else
            fail ("expected 2-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple2 #-}

-- | A 3-tuple decoder.
tuple3 :: (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
tuple3 f (Decoder fa) (Decoder fb) (Decoder fc) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 3
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
          else
            fail ("expected 3-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple3 #-}

-- | A 4-tuple decoder.
tuple4 :: (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
tuple4 f (Decoder fa) (Decoder fb) (Decoder fc) (Decoder fd) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 4
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
              <*> fd (Vector.unsafeIndex v 3)
          else
            fail ("expected 4-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple4 #-}

-- | A 5-tuple decoder.
tuple5 :: (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
tuple5 f (Decoder fa) (Decoder fb) (Decoder fc) (Decoder fd) (Decoder fe) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 5
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
              <*> fd (Vector.unsafeIndex v 3)
              <*> fe (Vector.unsafeIndex v 4)
          else
            fail ("expected 5-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple5 #-}

-- | A 6-tuple decoder.
tuple6 ::
  (a -> b -> c -> d -> e -> f -> g) ->
  Decoder a ->
  Decoder b ->
  Decoder c ->
  Decoder d ->
  Decoder e ->
  Decoder f ->
  Decoder g
tuple6 f (Decoder fa) (Decoder fb) (Decoder fc) (Decoder fd) (Decoder fe) (Decoder ff) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 6
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
              <*> fd (Vector.unsafeIndex v 3)
              <*> fe (Vector.unsafeIndex v 4)
              <*> ff (Vector.unsafeIndex v 5)
          else
            fail ("expected 6-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple6 #-}

-- | A 7-tuple decoder.
tuple7 ::
  (a -> b -> c -> d -> e -> f -> g -> h) ->
  Decoder a ->
  Decoder b ->
  Decoder c ->
  Decoder d ->
  Decoder e ->
  Decoder f ->
  Decoder g ->
  Decoder h
tuple7 f (Decoder fa) (Decoder fb) (Decoder fc) (Decoder fd) (Decoder fe) (Decoder ff) (Decoder fg) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 7
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
              <*> fd (Vector.unsafeIndex v 3)
              <*> fe (Vector.unsafeIndex v 4)
              <*> ff (Vector.unsafeIndex v 5)
              <*> fg (Vector.unsafeIndex v 6)
          else
            fail ("expected 7-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple7 #-}

-- | A 8-tuple decoder.
tuple8 ::
  (a -> b -> c -> d -> e -> f -> g -> h -> i) ->
  Decoder a ->
  Decoder b ->
  Decoder c ->
  Decoder d ->
  Decoder e ->
  Decoder f ->
  Decoder g ->
  Decoder h ->
  Decoder i
tuple8 f (Decoder fa) (Decoder fb) (Decoder fc) (Decoder fd) (Decoder fe) (Decoder ff) (Decoder fg) (Decoder fh) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 8
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
              <*> fd (Vector.unsafeIndex v 3)
              <*> fe (Vector.unsafeIndex v 4)
              <*> ff (Vector.unsafeIndex v 5)
              <*> fg (Vector.unsafeIndex v 6)
              <*> fh (Vector.unsafeIndex v 7)
          else
            fail ("expected 8-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple8 #-}

-- | A 9-tuple decoder.
tuple9 ::
  (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) ->
  Decoder a ->
  Decoder b ->
  Decoder c ->
  Decoder d ->
  Decoder e ->
  Decoder f ->
  Decoder g ->
  Decoder h ->
  Decoder i ->
  Decoder j
tuple9 f (Decoder fa) (Decoder fb) (Decoder fc) (Decoder fd) (Decoder fe) (Decoder ff) (Decoder fg) (Decoder fh) (Decoder fi) =
  Decoder
    ( Aeson.withArray "" \v ->
        if Vector.length v == 9
          then
            f
              <$> fa (Vector.unsafeIndex v 0)
              <*> fb (Vector.unsafeIndex v 1)
              <*> fc (Vector.unsafeIndex v 2)
              <*> fd (Vector.unsafeIndex v 3)
              <*> fe (Vector.unsafeIndex v 4)
              <*> ff (Vector.unsafeIndex v 5)
              <*> fg (Vector.unsafeIndex v 6)
              <*> fh (Vector.unsafeIndex v 7)
              <*> fi (Vector.unsafeIndex v 8)
          else
            fail ("expected 9-element array, but found " ++ show (Vector.length v))
    )
{-# INLINEABLE tuple9 #-}

-- | An object decoder.
object :: ObjectDecoder a -> Decoder a
object (ObjectDecoder f) =
  Decoder (Aeson.withObject "" f)
{-# INLINEABLE object #-}

-- | An object property decoder.
property :: Aeson.Key -> Decoder a -> ObjectDecoder a
property k (Decoder f) =
  ObjectDecoder \o -> Aeson.explicitParseField f o k
{-# INLINEABLE property #-}

-- | An optional object property decoder.
optionalProperty :: Aeson.Key -> Decoder a -> ObjectDecoder (Maybe a)
optionalProperty k (Decoder f) =
  ObjectDecoder \o -> Aeson.explicitParseFieldMaybe' f o k
{-# INLINEABLE optionalProperty #-}

-- | A map decoder.
map :: (Ord k) => (Aeson.Key -> k) -> Decoder a -> Decoder (Map k a)
map fromKey (Decoder f) =
  object (ObjectDecoder (Aeson.KeyMap.foldrWithKey (\k v -> liftA2 (Map.insert (fromKey k)) (f v)) (pure Map.empty)))
{-# INLINEABLE map #-}

-- | A key map decoder.
keyMap :: Decoder a -> Decoder (Aeson.KeyMap a)
keyMap (Decoder f) =
  object (ObjectDecoder (Aeson.KeyMap.traverse f))
{-# INLINEABLE keyMap #-}

-- | A null decoder.
null :: Decoder ()
null =
  Decoder \case
    Aeson.Null -> pure ()
    _ -> fail "expected null"
{-# INLINEABLE null #-}

-- | A nullable decoder.
nullable :: Decoder v -> Decoder (Maybe v)
nullable (Decoder f) =
  Decoder \case
    Aeson.Null -> pure Nothing
    val -> Just <$> f val
{-# INLINEABLE nullable #-}

-- | Refine a decoder with a predicate.
refine :: (a -> Either Text b) -> Decoder a -> Decoder b
refine p (Decoder f) =
  Decoder \val -> do
    x <- f val
    case p x of
      Left err -> fail (Text.unpack err)
      Right y -> pure y
{-# INLINEABLE refine #-}
