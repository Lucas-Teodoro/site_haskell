{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Setor where

import Import
import Text.Lucius

formSetor :: Form (Text,Text)
formSetor = renderDivs $ (,)
    <$> areq textField "Sigla: " Nothing
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
        FormSuccess (sg,setor) -> do
            _ <- runDB $ insert $ Setor sg setor True
            redirect HomeR
        _ -> redirect SetorR
            
getListaSetoresR :: Handler Html
getListaSetoresR = do 
    setores <- runDB $ selectList [] [Asc SetorDescSetor]
    defaultLayout $ do 
        addStylesheet $ StaticR css_menu2_css
        addStylesheet $ StaticR css_tabela_css
        toWidget $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/todossetores.hamlet")

