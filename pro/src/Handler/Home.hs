{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import System.FilePath
import Data.Text as A (unpack) 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

uploadForm :: Html -> MForm Handler (FormResult (FileInfo, Maybe Textarea, UTCTime), Widget)
uploadForm = renderBootstrap $ (,,)
    <$> fileAFormReq "Image file"
    <*> aopt textareaField "Image description" Nothing
    <*> lift (liftIO getCurrentTime)


getHomeR :: Handler Html
getHomeR = do
    ((_, widget), enctype) <- runFormPost uploadForm
    images <- runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate]
    mmsg <- getMessage
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost uploadForm
    case result of
        FormSuccess (file, info, date) -> do
            -- TODO: check if image already exists
            -- save to image directory
            filename <- writeToServer file
            _ <- runDB $ insert (Image filename info date)
            setMessage "Image saved"
            redirect HomeR
        _ -> do
            setMessage "Something went wrong"
            redirect HomeR



uploadDirectory :: FilePath
uploadDirectory = "files"

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = A.unpack $ fileName file
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]

