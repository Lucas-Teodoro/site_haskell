{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Processo where

import Text.Lucius
import Import

formProcesso :: Form Processo
formProcesso = renderBootstrap $ Processo 
    <$> areq textField "Autor: " Nothing 
    <*> areq intField "Ano: " Nothing
    <*> areq intField "Numero: " Nothing
    <*> areq textField "Assunto: " Nothing
    <*> areq dayField "Data de Criaçao: " Nothing
    <*> areq intField "Volume: " Nothing
    <*> areq (selectField (optionsPersistKey [] [Asc SetorId] setorDescSetor)) "Setor: " Nothing

getProcessoR :: Handler Html
getProcessoR = do 
    (widgetForm, enctype) <- generateFormPost formProcesso
    layoutAdmin $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/hamlet/processo.hamlet")
    
postProcessoR :: Handler Html 
postProcessoR = do
    ((res,_),_) <- runFormPost formProcesso
    case res of 
        FormSuccess processo -> do
            _ <- runDB $ insert processo
            redirect ProcessoR
        _ -> redirect ProcessoR