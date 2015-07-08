{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Fixed
import Data.IntSet (IntSet)
import Data.Time
import Data.Time.Clock.POSIX
import Language.Haskell.TH

import Unsafe.TrueName

-- I had an example for adding an @instance PrintfArg ()@,
-- but base-4.7.0.0 got there ahead of me.

main :: IO ()
main = do
    print $(ConE <$> trueName "Data.IntSet.Base.Nil" ''IntSet)
#if MIN_VERSION_containers(0,5,0)
    print $ $(ConE <$> trueName "Tip" ''IntSet) 0 31
#else
    print $ $(ConE <$> trueName "Tip" ''IntSet) 31
#endif

    -- same difference
    print ($(ConE <$> trueName "MkDiffTime" ''DiffTime) (7890.123456 :: Pico))
    print $ [quasiName| MkDiffTime ''DiffTime |] (7890.123456 :: Pico)

    -- patterns
    print . picoseconds =<< getPOSIXTime

picoseconds :: NominalDiffTime -> Integer
picoseconds dt = case dt of
    [quasiName| MkNominalDiffTime ''NominalDiffTime ps |] -> case ps of
#if MIN_VERSION_base(4,7,0)
        MkFixed n -> n
#else
        [quasiName| MkFixed ''Fixed n |] -> n
#endif

