{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Setor where

import Import
import Text.Lucius

formSetor :: Form Setor
formSetor = renderBootstrap $ Setor 
    <$> areq textField "Descricao do Sigla: " Nothing
    <*> areq textField "Descricao do Setor: " Nothing

getSetorR :: Handler Html
getSetorR = do 
    (widgetForm, enctype) <- generateFormPost formSetor
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/setor.hamlet")
    
postSetorR :: Handler Html 
postSetorR = do 
    ((res,_),_) <- runFormPost formSetor
    case res of 
        FormSuccess setor -> do
            _ <- runDB $ insert setor
            redirect SetorR
        x -> do 
            defaultLayout [whamlet|
                <h1>
                    #{show x}
            |]
