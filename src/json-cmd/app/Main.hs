{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (getContents)
import Lib ( Cmd (cmdExec)
           , parseCmd
           , setCmdArgs )
import System.Posix.Env.ByteString (getArgs)
import Data.ByteString.Lazy ( ByteString
                            , getContents
                            , fromStrict )
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (unpack)
import Control.Monad (liftM)
import System.Process ( CreateProcess
                      , shell
                      , readCreateProcess )


main :: IO ()
main = do input <- getContents
          argv <- getArgs
          let (cmdName:args) = map (decodeUtf8 . fromStrict) argv
              cmd = parseCmd cmdName input
          case cmd of
                  Nothing -> return ()
                  Just c -> do let shellCmd = cmdExec (setCmdArgs c args)
                                   creatProc = shell . unpack $ shellCmd
                                   result = readCreateProcess creatProc
                                                              (unpack . decodeUtf8 $ input)
                               putStr =<< result
