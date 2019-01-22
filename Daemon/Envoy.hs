module Daemon.Envoy where


import Network.Mail.SMTP
import Network.Mail.Mime (Part)

import Data.Text.Lazy (Text, pack, toStrict)

import Control.Monad
import Control.Applicative ((<|>))
import System.FilePath ((</>))
import System.Environment


type ID = String


watchQueue :: IO (ID, Message, String)
watchQueue = return ("test-id", Welcome, "team@purefunction-alchemy.com")


reportBackToQueue :: Maybe ID -> IO ()
reportBackToQueue _ = return ()


main = do
  host <- getEnv "MAIL_HOST"
  user <- getEnv "MAIL_USER"
  pass <- getEnv "MAIL_PASS"
  from <- getEnv "MAIL_FROM"
  name <- getEnv "MAIL_NAME"

  forever $   watchQueue
          >>= prepare (Address (Just $ text name) (text from))
          >>= try (sendMailWithLogin host user pass)
          >>= reportBackToQueue

  where

    prepare from (id, msg, to) = do
      parts <- contentFor msg
      return (id, simpleMail from [to'] [] [from] subject' parts)
      where
        subject' = text $ subject msg
        to' = Address Nothing $ text to

    try send (id, mail)  =  send mail >> return (Just id)
                        <|> return Nothing


data Message = Welcome
             | Bye
             deriving Show


subject Welcome = "Welcome, Stranger!"
subject Bye = "May the λ be with you!"


contentFor :: Message -> IO [Part]
contentFor = sequence . fmap load . parts . show
  where
    load (f,p) = do
      s <- readFile $ templatesDir </> f
      return . p $ pack s
    parts name = [ (name ++ ".txt" , plainTextPart)
                 , (name ++ ".html", htmlPart)
                 ]


text = toStrict . pack


root = "Daemon" </> "Envoy"

templatesDir = root </> "Template"
