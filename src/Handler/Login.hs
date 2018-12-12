{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Lucius

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    (widgetForm, enctype) <- generateFormPost formLogin
    mensagem <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/login.hamlet")

postLoginR :: Handler Html 
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess (email,senha) -> do
            logado <- runDB $ selectFirst [UsuarioEmail ==. email,
                                          UsuarioSenha ==. senha] []
            case logado of
                Just (Entity _ (Usuario a b c d e f)) -> do
                    case (Just e) of
                        Just False -> do
                            setSession "_USR" (pack $ show (Usuario a b c d e f))
                            setMessage [shamlet|
                                <h6>
                                    Usuario logado
                            |]
                            redirect HomeR
                        _ -> do
                            setSession "_USR" "ADMIN"
                            setMessage [shamlet|
                                <h6>
                                    Administrador logado
                            |]  
                            redirect HomeR
                _ -> do
                    setMessage [shamlet|
                        <h6>
                            Usuario e senha nao encontrados!
                    |]
                    redirect LoginR
        _ -> redirect HomeR            

postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_USR"
    redirect SetorR
    
getLogoutR:: Handler Html
getLogoutR = do
    deleteSession "_USR"
    redirect SetorR
