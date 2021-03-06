{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Text.Lucius
import Import

getHomeR :: Handler Html
getHomeR = do
    mensagem <- getMessage
    logado <- lookupSession "_USR"
    listaprocessos <- runDB $ selectList [] [Desc ProcessoCriacao]
    let processos = take 5 listaprocessos
    case logado of
        Nothing -> do
            layoutPublic $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/homeprocesso.hamlet")
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/homeprocesso.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/homeprocesso.hamlet")
                        