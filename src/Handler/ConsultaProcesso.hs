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
        $(whamletFile "templates/hamlet/formconsultanome.hamlet")

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
                $(whamletFile "templates/hamlet/consultaprocesso.hamlet")
        _ -> redirect HomeR
