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
    logado <- lookupSession "_USR"
    mensagem <- getMessage
    case logado of
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/setor.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/setor.hamlet")

    
postSetorR :: Handler Html 
postSetorR = do 
    ((res,_),_) <- runFormPost formSetor
    case res of 
        FormSuccess (sg,setor) -> do
            _ <- runDB $ insert $ Setor sg setor True
            setMessage [shamlet|
                <h6>
                    Setor cadastrado com sucesso
            |]
            redirect ListaSetoresR
        _ -> redirect HomeR

getListaSetoresR :: Handler Html
getListaSetoresR = do 
    logado <- lookupSession "_USR"
    setores <- runDB $ selectList [] [Asc SetorDescSetor]
    case logado of
        Just "ADMIN" -> do
            layoutAdmin $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/todossetores.hamlet")
        _ -> do
            layoutUser $ do
                addStylesheet $ StaticR css_menu2_css
                addStylesheet $ StaticR css_tabela_css
                toWidget $(luciusFile "templates/homepage.lucius")
                $(whamletFile "templates/todossetores.hamlet")
