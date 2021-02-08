module Squid.Prelude
  ( module Exports
  , symbolText
  ) where

import Control.Applicative    as Exports (empty, pure)
import Data.Text              as Exports (Text, pack)
import Data.Maybe             as Exports (catMaybes)
import Data.List.NonEmpty     as Exports (NonEmpty)
import Data.Kind              as Exports
import Data.Text.Encoding     as Exports (decodeUtf8, encodeUtf8)
import GHC.Generics           as Exports (Generic)
import GHC.Natural            as Exports (Natural)
import GHC.TypeLits           as Exports
import Data.Proxy             as Exports


symbolText :: forall s . (KnownSymbol s) => Text
symbolText = pack (symbolVal (Proxy @s))
