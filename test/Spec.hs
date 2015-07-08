module Main where

-- | Unfortunately there is no test. There is only use cases
-- where you can find how you can use telegram bot api methods

import Control.Lens.Basic
import Data.Text (Text)
import Control.Applicative ((<$>))
import Data.Maybe (maybe)
import           Data.Configurator         (Worth (Required))
import qualified Data.Configurator         as C
import           Data.Configurator.Types   (Name)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO, MonadIO)

import TelegramBotAPI


getToken :: Name -> IO (Maybe Token)
getToken name = C.load [Required "bot.config"] >>=
    \config ->
         C.lookup config (T.append name ".token")


getTelegramToken :: MonadIO m => m (Maybe Token)
getTelegramToken = liftIO (getToken "telegram")


getUpdateId :: Update -> Int
getUpdateId (Update u) = view @updateId u

getMessage :: Update -> Maybe Message
getMessage (Update u) = view @message u


showMessage :: Message -> String
showMessage (Message m) = "MessageId: " ++
    show (view @messageId m) ++
    "\nFrom: " ++
    show (view @from m) ++
    "\nText: " ++
    show (view @text m) ++
    "\nPhoto: " ++
    show (view @photo m) ++
    "\n"

main :: IO ()
main = do
    Just token <- getTelegramToken
    config <- createTelegramConfig token
    me <- runTelegramAPI config getMe
    print me
    updates <-
        runTelegramAPI
            config
            (getUpdates Nothing Nothing Nothing)
    mapM_
        (\update ->
              putStrLn
                  ("update_id: " ++
                   (show (getUpdateId update)) ++
                   "\n" ++
                   (maybe "" showMessage (getMessage update))))
        updates
    m1 <-
        runTelegramAPI
            config
            (sendMessage
                 111874928
                 "Hi from telegram-client"
                 Nothing
                 Nothing
                 Nothing)
    print (showMessage m1)
