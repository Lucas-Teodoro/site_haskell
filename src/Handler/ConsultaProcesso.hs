{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.ConsultaProcesso where

import Text.Lucius
import Import

dateFormat :: Day -> String
dateFormat = formatTime defaultTimeLocale "%F"

formConsulta :: Form (Text,Int)
formConsulta = renderDivs $ (,) 
    <$> areq textField "Autor: " Nothing
    <*> areq intField "Ano do Processo: " Nothing

getConsultaPorAutorR :: Handler Html
getConsultaPorAutorR = do 
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta
    case logado of
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formconsultanome.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formconsultanome.hamlet")

postConsultaPorAutorR :: Handler Html
postConsultaPorAutorR = do
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta
    case res of 
        FormSuccess (autor,_) -> do
            processos <- runDB $ selectList [Filter ProcessoAutor (Left $  concat ["%", autor, "%"]) (BackendSpecificFilter "ILIKE")] []
            case logado of
                Just "ADMIN" -> do
                    layoutAdmin $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/consultaprocesso.hamlet")
                _ -> do
                    layoutUser $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/consultaprocesso.hamlet")
        _ -> redirect HomeR

getCPPorAutorR :: Handler Html
getCPPorAutorR = do 
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta
    case logado of
        Nothing -> do
            layoutPublic $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formcpnome.hamlet")
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formcpnome.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formcpnome.hamlet")
                        
postCPPorAutorR :: Handler Html
postCPPorAutorR = do
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta
    case res of 
        FormSuccess (autor,_) -> do
            processos <- runDB $ selectList [Filter ProcessoAutor (Left $  concat ["%", autor, "%"]) (BackendSpecificFilter "ILIKE")] []
            case logado of
                Nothing -> do
                    layoutPublic $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/cpprocessos.hamlet")
                Just "ADMIN" -> do
                    layoutAdmin $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/cpprocessos.hamlet")
                _ -> do
                    layoutUser $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/cpprocessos.hamlet")
        _ -> redirect HomeR

formConsulta' :: Form (Int,Int)
formConsulta' = renderDivs $ (,) 
    <$> areq intField "Numero: " Nothing
    <*> areq intField "Ano" Nothing

getConsultaPorNumR :: Handler Html
getConsultaPorNumR = do 
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta
    case logado of
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formconsultanum.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formconsultanum.hamlet")

postConsultaPorNumR :: Handler Html
postConsultaPorNumR = do
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta'
    case res of 
        FormSuccess (numero,ano) -> do
            processos <- runDB $ selectList [ProcessoAno ==. ano, ProcessoNumero ==. numero] [Asc ProcessoId]
            case logado of
                Just "ADMIN" -> do
                    layoutAdmin $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/consultaprocesso.hamlet")
                _ -> do
                    layoutUser $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/consultaprocesso.hamlet")
        _ -> redirect HomeR



getCPPorNumR :: Handler Html
getCPPorNumR = do 
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta'
    case logado of
        Nothing -> do
            layoutPublic $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formcpnum.hamlet")
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formcpnum.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/formcpnum.hamlet")

postCPPorNumR :: Handler Html
postCPPorNumR = do
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta'
    case res of 
        FormSuccess (numero,ano) -> do
            processos <- runDB $ selectList [ProcessoAno ==. ano, ProcessoNumero ==. numero] [Asc ProcessoId]
            case logado of
                Nothing -> do
                    layoutPublic $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/cpprocessos.hamlet")
                Just "ADMIN" -> do
                    layoutAdmin $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/cpprocessos.hamlet")
                _ -> do
                    layoutUser $ do
                        addStylesheet $ StaticR css_menu2_css
                        addStylesheet $ StaticR css_tabela_css
                        toWidget $(luciusFile "templates/homepage.lucius")
                        $(whamletFile "templates/cpprocessos.hamlet")
        _ -> redirect HomeR
