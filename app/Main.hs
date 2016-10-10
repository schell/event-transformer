{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Trans.Event
import Control.Monad.IO.Class
import Data.Function (fix)

mainEvent :: MonadIO m => EventT m ()
mainEvent = do
  liftIO $ putStrLn "Welcome to an EventT example. Type quit(enter) to quit."
  fix $ \loop -> do
    ln <- liftIO getLine
    if ln == "quit"
      then done ()
      else do
        liftIO $ putStrLn $ "> " ++ ln
        next loop
  liftIO $ putStrLn "Bye!"

loop :: EventT IO () -> IO ()
loop e = runEventT e >>= \case
  Left ()   -> return ()
  Right nxt -> loop nxt

main :: IO ()
main = loop mainEvent
