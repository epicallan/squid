{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Squid.DataBase.HasNumericalType where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind
import Data.Proxy
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural

class HasNumericalType (a :: Type) where
  isNumerical :: Proxy a -> Bool

instance (NumericalPred a flag, HasNumericalType' a flag) => HasNumericalType a where
  isNumerical proxy = isNumerical' proxy (Proxy @flag)

class HasNumericalType' (a :: Type) (flag :: Bool) where
  isNumerical' :: Proxy a -> Proxy flag -> Bool

instance HasNumericalType' a 'True where
  isNumerical' _ _ = True

instance HasNumericalType' a 'False where
  isNumerical' _ _ = False

class NumericalPred a flag | a -> flag where

instance (flag ~ 'False) => NumericalPred a flag

instance {-#Overlapping#-} NumericalPred Int 'True
instance {-#Overlapping#-} NumericalPred Int64 'True
instance {-#Overlapping#-} NumericalPred Int8 'True
instance {-#Overlapping#-} NumericalPred Int32 'True
instance {-#Overlapping#-} NumericalPred Int16 'True
instance {-#Overlapping#-} NumericalPred Word 'True
instance {-#Overlapping#-} NumericalPred Word64 'True
instance {-#Overlapping#-} NumericalPred Word8 'True
instance {-#Overlapping#-} NumericalPred Word32 'True
instance {-#Overlapping#-} NumericalPred Word16 'True
instance {-#Overlapping#-} NumericalPred Natural 'True
