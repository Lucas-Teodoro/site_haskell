{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Teste where

import Import
import Text.Lucius
import Handler.Setor(formSetor)

getTesteR :: Handler Html
getTesteR = do
    (widgetForm, enctype) <- generateFormPost formSetor
    defaultLayout $ do
        addStylesheet $ StaticR css_menu2_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/setor.hamlet")
        
postTesteR :: Handler Html 
postTesteR = do 
    ((res,_),_) <- runFormPost formSetor
    case res of 
        FormSuccess _ -> do
            redirect HomeR
        x -> do 
            defaultLayout [whamlet|
                <h1>
                    #{show x}
            |]