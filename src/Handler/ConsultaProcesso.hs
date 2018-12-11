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
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/formconsultanome.hamlet")

postConsultaPorAutorR :: Handler Html
postConsultaPorAutorR = do
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta
    case res of 
        FormSuccess (autor,_) -> do
            processos <- runDB $ selectList [Filter ProcessoAutor (Left $  concat ["%", autor, "%"]) (BackendSpecificFilter "ILIKE")] []
            defaultLayout $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/consultaprocesso.hamlet")
        _ -> redirect HomeR

getCPPorAutorR :: Handler Html
getCPPorAutorR = do 
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/formcpnome.hamlet")

postCPPorAutorR :: Handler Html
postCPPorAutorR = do
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta
    case res of 
        FormSuccess (autor,_) -> do
            processos <- runDB $ selectList [Filter ProcessoAutor (Left $  concat ["%", autor, "%"]) (BackendSpecificFilter "ILIKE")] []
            defaultLayout $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/consultaprocesso.hamlet")
        _ -> redirect HomeR

formConsulta' :: Form (Int,Int)
formConsulta' = renderDivs $ (,) 
    <$> areq intField "Numero: " Nothing
    <*> areq intField "Ano" Nothing

getConsultaPorNumR :: Handler Html
getConsultaPorNumR = do 
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/formconsultanum.hamlet")

postConsultaPorNumR :: Handler Html
postConsultaPorNumR = do
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta'
    case res of 
        FormSuccess (numero,ano) -> do
            processos <- runDB $ selectList [ProcessoAno ==. ano, ProcessoNumero ==. numero] [Asc ProcessoId]
            defaultLayout $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/consultaprocesso.hamlet")
        _ -> redirect HomeR

getCPPorNumR :: Handler Html
getCPPorNumR = do 
    mensagem <- getMessage
    (widgetForm, enctype) <- generateFormPost formConsulta'
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/formcpnum.hamlet")

postCPPorNumR :: Handler Html
postCPPorNumR = do
    mensagem <- getMessage
    ((res,_),_) <- runFormPost formConsulta'
    case res of 
        FormSuccess (numero,ano) -> do
            processos <- runDB $ selectList [ProcessoAno ==. ano, ProcessoNumero ==. numero] [Asc ProcessoId]
            defaultLayout $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/consultaprocesso.hamlet")
        _ -> redirect HomeR
