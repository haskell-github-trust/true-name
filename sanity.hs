{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Concurrent.Chan
import Control.Concurrent.MVar
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
#if MIN_VERSION_containers(0,5,0)
    print $(ConE <$> summon "Data.IntSet.Base.Nil" ''IntSet)
    print [truename| ''IntSet Data.IntSet.Base.Nil |] -- same
    print $ $(ConE <$> summon "Tip" ''IntSet) 0 31
#else
    print $(ConE <$> summon "Data.IntSet.Nil" ''IntSet)
    print [truename| ''IntSet Data.IntSet.Nil |] -- same
    print $ $(ConE <$> summon "Tip" ''IntSet) 31
#endif

    -- same difference
    print ($(ConE <$> summon "MkDiffTime" ''DiffTime) (7890.123456 :: Pico))
    print $ [truename| ''DiffTime MkDiffTime |] (7890.123456 :: Pico)

    -- patterns
    print . picoseconds =<< getPOSIXTime

    -- go deeper; refer to "Control.Concurrent.Chan"
    chan@[truename| ''Chan Chan | chanR chanW |]
        <- newChan :: IO ([truename| 'newChan Chan |] Int)
        -- get at 'Chan' via the type of 'newChan'
    writeChan chan (42 :: Int)

    streamR <- readMVar chanR :: IO ([truename| ''Chan Chan Stream |] Int)
    [truename| ''Chan Chan Stream ChItem ChItem | x next |] <- readMVar streamR
        :: IO ([truename| ''Chan Chan Stream ChItem |] Int)
    putStrLn $ "chan contains: " ++ show x

    streamW <- readMVar chanW :: IO ([truename| ''Chan Chan Stream |] Int)
    putStrLn $ "next == streamW? " ++ show (next == streamW)

picoseconds :: NominalDiffTime -> Integer
picoseconds dt = case dt of
    [truename| ''NominalDiffTime MkNominalDiffTime | ps |] -> case ps of
#if MIN_VERSION_base(4,7,0)
        MkFixed n -> n
#else
        [truename| ''Fixed MkFixed | n |] -> n
#endif

