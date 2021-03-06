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
    logado <- lookupSession "_USR"
    (widgetForm, enctype) <- generateFormPost formUsuario
    mensagem <- getMessage
    case logado of
        Nothing -> redirect HomeR
        _ -> do
            layoutAdmin $ do 
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
                    <h6>
                        Usuario cadastrado
                |]
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <h6>
                        Senha e confirmacao não conferem
                |]
            redirect UsuarioR
        _ -> redirect UsuarioR

getListaUsuariosR :: Handler Html
getListaUsuariosR = do 
    logado <- lookupSession "_USR"
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    case logado of
        Just _ -> do
            layoutAdmin $ do 
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/todosusuarios.hamlet")
        Nothing -> redirect HomeR
        
