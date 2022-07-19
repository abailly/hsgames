{-# LANGUAGE MultiParamTypeClasses #-}

module GameServer.App.HTML where

import Network.Wai (Application)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)

userInterface :: FilePath -> Application
userInterface path = staticApp (defaultFileServerSettings path)
