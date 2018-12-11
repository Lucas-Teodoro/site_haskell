{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just HomeR
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _ _ = return Authorized

instance Yesod App where
    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

ehAdmin :: Handler AuthResult
ehAdmin = do 
    logado <- lookupSession "_USR"
    case logado of 
        Just usuario -> do
            setorLog <- return $ usuarioSetor $ read $ unpack usuario
            permissao <- return $ usuarioPermissao $ read $ unpack usuario
--                  True == Admin 
            if permissao == True then 
                return Authorized
            else 
                return $ Unauthorized "Acesso negado!"
        Nothing -> return AuthenticationRequired

ehUsuario :: Handler AuthResult
ehUsuario = do 
    logado <- lookupSession "_USR"
    case logado of 
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired
