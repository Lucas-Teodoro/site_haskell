{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import Prelude              (read)

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
    isAuthorized HomeR _ = return Authorized
    isAuthorized CPPorAutorR _ = return Authorized
    isAuthorized CPPorNumR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized LogoutR _ = ehUsuario
    isAuthorized ListaSetoresR _ = ehUsuario
    isAuthorized _ _ = ehAdmin

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

layoutUser :: Widget -> Handler Html
layoutUser widget = do
    pc <- widgetToPageContent $ do
        widget
        toWidget [lucius| body { font-family: verdana } |]
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>GPEPP - Gerenciador de Processos para Empresas de Pequeno Porte
                    <meta charset=utf-8>
                    ^{pageHead pc}
                <body>
                    <div.principal>
                        <h1>
                            <a.topo href=@{HomeR}>
                                Gerenciador&nbsp;de&nbsp;Processos&nbsp;para&nbsp;Empresas&nbsp;de&nbsp;Pequeno&nbsp;Porte
                        <div.menu>
                            <ul>
                                <li class="dropdown">
                                    <div.dropbtn>
                                        <a href="#">
                                            Processo
                                        <div class="dropdown-content">
                                            <a href=@{ConsultaPorAutorR}>
                                                Consulta&nbsp;de&nbsp;Processo&nbsp;por&nbsp;Autor
                                            <a href=@{ConsultaPorNumR}>
                                                Consulta&nbsp;de&nbsp;Processo&nbsp;por&nbsp;Numero 
                                <li class="dropdown">
                                    <div.dropbtn>
                                        <a href="#">
                                            Usuarios
                                        <div.dropdown-content>
                                            <a href=@{ListaUsuariosR}>
                                                Meu&nbsp;Perfil
                                <li class="dropdown">
                                    <div.dropbtn>
                                        <a href="#">
                                            Setores
                                        <div.dropdown-content>                                            
                                            <a href=@{ListaSetoresR}>
                                                Consulta&nbsp;de&nbsp;Setores
                                <li class="dropdown">
                                    <div.dropbtn>
                                        <a href="#">
                                            Solicitações
                                        <div.dropdown-content>                                            
                                            <a href=@{HomeR}>
                                                Consulta&nbsp;de&nbsp;Solicitacoes
                                <li class="dropdown">
                                    <div.dropbtn>
                                        <a href=@{LogoutR}>
                                            Logout
                    <div.container>
                        ^{pageBody pc}
         |]

layoutAdmin:: Widget -> Handler Html
layoutAdmin widget = do
    pc <- widgetToPageContent $ do
        widget
        toWidget [lucius| body { font-family: verdana } |]
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>GPEPP - Gerenciador de Processos para Empresas de Pequeno Porte
                    <meta charset=utf-8>
                    ^{pageHead pc}
                <body>
                    <div.principal>
                        <h1>
                            <a href=@{HomeR}>
                                Gerenciador&nbsp;de&nbsp;Processos&nbsp;para&nbsp;Empresas&nbsp;de&nbsp;Pequeno&nbsp;Porte
                        <div.menu>
                            <ul>
                                <li.dropdown>
                                    <div.dropbtn>
                                        <a href="#">
                                            Processo
                                        <div.dropdown-content>
                                            <a href=@{ProcessoR}>
                                                Cadastro&nbsp;de&nbsp;Processo
                                            <a href=@{ConsultaPorAutorR}>
                                                Consulta&nbsp;de&nbsp;Processo&nbsp;por&nbsp;Autor
                                            <a href=@{ConsultaPorAutorR}>
                                                Consulta&nbsp;de&nbsp;Processo&nbsp;por&nbsp;Numero 
                                <li.dropdown>
                                    <div.dropbtn>
                                        <a href="#">
                                            Usuarios
                                        <div.dropdown-content>
                                            <a href=@{UsuarioR}>
                                                Cadastro&nbsp;de&nbsp;Usuarios
                                            <a href=@{ListaUsuariosR}>
                                                Consulta&nbsp;de&nbsp;Usuarios
                                <li.dropdown>
                                    <div.dropbtn>
                                        <a href="#">
                                            Setores
                                        <div.dropdown-content>                                            
                                            <a href=@{SetorR}>
                                                Cadastro&nbsp;de&nbsp;Setor
                                            <a href=@{ListaSetoresR}>
                                                Consulta&nbsp;de&nbsp;Setores
                                <li.dropdown>
                                    <div.dropbtn>
                                        <a href="#">
                                            Solicitações
                                        <div.dropdown-content>                                            
                                            <a href=@{HomeR}>
                                                Consulta&nbsp;de&nbsp;Solicitacoes
                                <li.dropdown>
                                    <div.dropbtn>
                                        <a href=@{LogoutR}>
                                            Logout
                    <div.container>
                        ^{pageBody pc}
         |]
         
layoutPublic :: Widget -> Handler Html
layoutPublic widget = do
    pc <- widgetToPageContent $ do
        widget
        toWidget [lucius| body { font-family: verdana } |]
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>GPEPP - Gerenciador de Processos para Empresas de Pequeno Porte
                    <meta charset=utf-8>
                    ^{pageHead pc}
                <body>
                    <div.principal>
                        <h1>
                            <a href=@{HomeR}>
                                Gerenciador&nbsp;de&nbsp;Processos&nbsp;para&nbsp;Empresas&nbsp;de&nbsp;Pequeno&nbsp;Porte
                        <div.menu>
                            <ul>
                                <li.dropdown>
                                    <div.dropbtn>
                                        <a href="#">
                                            Consulta&nbsp;de&nbsp;Processo
                                        <div.dropdown-content>
                                            <a href=@{CPPorAutorR}>
                                                Por&nbsp;Autor
                                            <a href=@{CPPorNumR}>
                                                Por&nbsp;Número
                                 <li.dropdown>
                                    <div.dropbtn>
                                        <a href=@{LoginR}>
                                            Login
                    <div.container>
                        ^{pageBody pc}
         |]
