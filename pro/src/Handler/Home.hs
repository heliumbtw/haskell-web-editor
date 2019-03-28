{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import System.FilePath
import Database.Persist
import Graphics.Image.Processing
import Graphics.Image
import Yesod.Static
import Graphics.Image.IO
import Database.Persist.Sqlite
import System.Directory (removeFile, doesFileExist)
import Data.Text as A (unpack) 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

staticFiles "static"
uploadForm :: Html -> MForm Handler (FormResult (FileInfo, UTCTime), Widget)
uploadForm = renderBootstrap $ (,)
    <$> fileAFormReq "Image file"
    <*> lift (liftIO getCurrentTime)
   
getHomeR :: Handler Html
getHomeR = do
    ((_, widget), enctype) <- runFormPost uploadForm
    images <- runDB $ selectList [Image2Filename !=. ""] [Desc Image2Date,LimitTo 1]
    defaultLayout $ do
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost uploadForm
    case result of
        FormSuccess (file, date) -> do
            filename <- writeToServer file
            _ <- runDB $ insert (Image2 filename date)
            setMessage "Image saved"
            redirect HomeR
        _ -> do
            setMessage "Something went wrong"
            redirect HomeR

getFifthR :: Handler Html
getFifthR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGB VU  "static/img/cat.jpg"
           let [frog_red, frog_green, frog_blue] = toImagesX  frog
           liftIO $ writeImage "static/img/frog2.jpg" $ toImageY frog_red
           defaultLayout $ do
                --saddScript $ StaticR Import.js_refresh_js
                $(widgetFile "homepage")

getCGrayR :: Handler Html
getCGrayR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGB VU  "static/img/cat.jpg"
           let [frog_red, frog_green, frog_blue] = toImagesX  frog
           liftIO $ writeImage "static/img/frog2.jpg" $ toImageY frog_green
           defaultLayout $ do
                $(widgetFile "homepage")

getCstGrayR :: Handler Html
getCstGrayR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGB VU  "static/img/cat.jpg"
           let [frog_red, frog_green, frog_blue] = toImagesX  frog
           liftIO $ writeImage "static/img/frog2.jpg" $ toImageY frog_blue
           defaultLayout $ do
                $(widgetFile "homepage")

getFlipHR :: Handler Html
getFlipHR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGBA VU  "static/img/cat.jpg"
           liftIO $ writeImage "static/img/frog2.jpg" $ flipH frog
           defaultLayout $ do
                $(widgetFile "homepage")

getFlipVR :: Handler Html
getFlipVR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGBA VU  "static/img/cat.jpg"
           liftIO $ writeImage "static/img/frog2.jpg" $ flipV frog
           defaultLayout $ do
                $(widgetFile "homepage")

getLtrR :: Handler Html
getLtrR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGBA VU  "static/img/cat.jpg"
           liftIO $ writeImage "static/img/frog2.jpg" $ leftToRight frog frog
           defaultLayout $ do
                $(widgetFile "homepage")

getTtbR :: Handler Html
getTtbR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGBA VU  "static/img/cat.jpg"
           liftIO $ writeImage "static/img/frog2.jpg" $ topToBottom frog frog
           defaultLayout $ do
                $(widgetFile "homepage")

getNegativeR :: Handler Html
getNegativeR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGB VS  "static/img/cat.jpg"
           liftIO $ writeImageExact PNG [] "static/img/frog2.png" $ thresholdWith (PixelRGB (>0.55) (<0.6) (<0.5)) frog
           defaultLayout $ do
                $(widgetFile "homepage")

getRgbR :: Handler Html
getRgbR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGB VU  "static/img/cat.jpg"
           let [frog_red, frog_green, frog_blue] = toImagesX  frog
           liftIO $ writeImage "static/img/frog2.jpg" $ fromImagesX [(RedRGB, frog_red), (BlueRGB, frog_green), (GreenRGB, frog_blue)]
           defaultLayout $ do
                $(widgetFile "homepage")

getRotateR :: Handler Html
getRotateR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGBA VU  "static/img/cat.jpg"
           liftIO $ writeImage "static/img/frog2.jpg" $ rotate Bilinear (Fill 0) (11*pi/6) frog
           defaultLayout $ do
                $(widgetFile "homepage")

getSobopR :: Handler Html
getSobopR = do
           ((result, widget), enctype) <- runFormPost uploadForm
           frog <- liftIO $ readImageRGBA VU  "static/img/cat.jpg"
           let frogX = convolve Edge (fromLists [[1, 0, -1], [2, 0, -2], [1, 0, -1]]) frog
           let frogY = convolve Edge (fromLists [[1 ,2 ,1], [ 0, 0, 0], [ -1, -2, -1]]) frog
           liftIO $ writeImage "static/img/frog2.jpg" $ sqrt (frogX ^ 2 + frogY ^ 2)
           defaultLayout $ do
                $(widgetFile "homepage")

uploadDirectory :: FilePath
uploadDirectory = "static/img"

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = "cat.jpg"
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

