{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( Dep(..)
    , Cmd(..)
    , parseCmd
    , setCmdArgs
    ) where

import GHC.Generics (Generic)
import Data.Char (toLower)
import Data.Text.Lazy ( Text
                      , replace )
import Data.ByteString.Lazy (ByteString)
import Data.List (find)
import Data.Aeson ( (.:)
                  , decode )
import Data.Aeson.Types ( Parser
                        , FromJSON (parseJSON)
                        , Options ( constructorTagModifier
                                  , fieldLabelModifier )
                        , Object
                        , genericParseJSON
                        , parseMaybe
                        , defaultOptions
                        , camelTo )
import Control.Monad (mzero)


{--- JSON Database Format Specification ---}

data Dep = Dep
         { depBin :: Text
         , depVersion :: Text
         } deriving (Generic, Show)

instance FromJSON Dep where
        parseJSON = genericParseJSON $ optionsWithPrefix "dep"

data Cmd = Cmd
         { cmdName :: Text
         , cmdArgs :: Maybe [Text]
         , cmdDeps :: Maybe [Dep]
         , cmdExec :: Text
         } deriving (Generic, Show)

instance FromJSON Cmd where
        parseJSON = genericParseJSON $ optionsWithPrefix "cmd"

data CmdDb = CmdDb
         { cmdDbVersion :: Text
         , cmdDbCmdList :: [Cmd]
         } deriving (Generic, Show)

instance FromJSON CmdDb where
        parseJSON = genericParseJSON $ optionsWithPrefix "cmdDb"

cmdDb :: Object -> Parser CmdDb
cmdDb = (.: "json-cmd-db")


dropPrefix :: String -> String -> String
dropPrefix prefix = lowerHead . drop (length prefix)
        where lowerHead :: String -> String
              lowerHead (h:ss) = toLower h : ss

optionsWithPrefix :: String -> Options
optionsWithPrefix prefix =
        defaultOptions { fieldLabelModifier = camelTo '-' . dropPrefix prefix }


{--- Cmd Processing ---}

findCmd :: Text -> [Cmd] -> Maybe Cmd
findCmd = find . isNameofCmd
        where isNameofCmd :: Text -> Cmd -> Bool
              isNameofCmd name cmd = name == cmdName cmd

parseCmd :: Text -> ByteString -> Maybe Cmd
parseCmd cmd json = do obj <- decode json
                       db <- parseMaybe cmdDb obj
                       findCmd cmd (cmdDbCmdList db)


setCmdArgs :: Cmd -> [Text] -> Cmd
setCmdArgs cmd args = case cmdArgs cmd of
        Nothing   -> cmd
        Just tags -> let argSetters = zipWith replace tags args
                     in cmd { cmdArgs = Just args
                            , cmdExec = foldr ($) (cmdExec cmd) argSetters
                            }
