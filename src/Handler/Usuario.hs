{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius

formUsuario :: Form (Usuario,Text)
formUsuario = renderDivs $ (,) 
    <$> (Usuario 
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "Email: " Nothing
        <*> areq passwordField "Senha: " Nothing
        <*> areq (selectField (optionsPersistKey [SetorAtivo ==. True] [Asc SetorDescSetor] setorDescSetor)) "Setor: " Nothing
        <*> areq checkBoxField "Permissão de Administrador: " Nothing
        <*> areq checkBoxField  "Usuario Ativo" (Just True)
        )
    <*> areq passwordField "Confirmacao de Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widgetForm, enctype) <- generateFormPost formUsuario
    mensagem <- getMessage
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/usuario.hamlet")
    
postUsuarioR :: Handler Html 
postUsuarioR = do 
    ((res,_),_) <- runFormPost formUsuario
    case res of 
        FormSuccess (usuario,confirmacao) -> do
            if (usuarioSenha usuario == confirmacao) then do
                _ <- runDB $ insert usuario
                setMessage [shamlet|
                    <h1>
                        Usuario cadastrado
                |]
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <h1>
                        Senha e confirmacao não conferem
                |]
            redirect UsuarioR
        _ -> redirect UsuarioR
