{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.Log
  ( initLogger ) where

import           Control.Applicative
import           Control.Monad             (liftM)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Configurator
import           Paths_snaplet_hslogger
import           Snap.Snaplet
import           System.Log
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

-- | Initialize the Logger Snaplet.
--
-- No custom `v` type here (just Unit) because the logger state is
-- globally maintained anyway and this snaplet is just providing an
-- easy default configuration for HSLogger.
initLogger :: SnapletInit b ()
initLogger = makeSnaplet "hslogger" description datadir $ do
    conf <- getSnapletUserConfig
    liftIO $ do
     logname   <- require conf "default_logger"
     loglevel  <- require conf "log_level"
     logformat <- require conf "log_format"
     logfile   <- require conf "log_file"
     let lvl = read loglevel :: Priority
     h <- flip setFormatter (simpleLogFormatter logformat)
            <$> fileHandler logfile lvl
     updateGlobalLogger rootLoggerName removeHandler
     updateGlobalLogger logname (setLevel lvl . setHandlers [h])
     noticeM logname $ "Configured the logger with a level of: " ++ show lvl
  where
    description = "Snaplet for HSLogger library"
    datadir = Just $ liftM (++"/resources/hslogger") getDataDir
