module Cretheus.Encode
  ( Target,

    -- * Encoders
    bool,
    int,
    text,
    list,
    vector,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

class Target a where
  bool_ :: Bool -> a
  int_ :: Int -> a
  text_ :: Text -> a
  list_ :: (x -> a) -> [x] -> a
  vector_ :: (x -> a) -> Vector x -> a
  object_ :: [(Aeson.Key, a)] -> a

instance Target Aeson.Value where
  bool_ = Aeson.toJSON
  int_ = Aeson.toJSON
  text_ = Aeson.toJSON
  list_ f = Aeson.toJSON . map f
  vector_ f = Aeson.Array . Vector.map f
  object_ = Aeson.Object . Aeson.KeyMap.fromList

instance Target Aeson.Encoding where
  bool_ = Aeson.bool
  int_ = Aeson.int
  text_ = Aeson.text
  list_ = Aeson.list
  vector_ f = Aeson.list f . Vector.toList
  object_ = Aeson.pairs . foldMap (\(k, v) -> Aeson.pair k v)

type Encoder a =
  forall x. Target x => a -> x

bool :: Encoder Bool
bool = bool_

int :: Encoder Int
int = int_

text :: Encoder Text
text = text_

list :: Encoder a -> Encoder [a]
list e = list_ e

vector :: Encoder a -> Encoder (Vector a)
vector e = vector_ e

object :: Target a => [(Aeson.Key, a)] -> a
object = object_
