{-# LANGUAGE CPP #-}

module Text.Megaparsec.Compat
    ( module X
#if !MIN_VERSION_megaparsec(7,0,0)
    , errorBundlePretty
#endif
    ) where

import Text.Megaparsec as X

#if !MIN_VERSION_megaparsec(7,0,0)
import Prelude

errorBundlePretty :: Show a => a -> String
errorBundlePretty = show
#endif
