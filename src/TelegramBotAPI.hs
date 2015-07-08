module TelegramBotAPI where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson
       (ToJSON, FromJSON, parseJSON, Value(..), (.:), (.:?), (.=), decode,
        encode, toJSON, object)
import Data.Aeson.Types (Result(..), parse)
import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BL
import Data.List (lookup)
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Typeable (Typeable)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prelude hiding (concat, id)
import Data.Text (Text)
import Data.HashMap.Lazy (member)
import Control.Lens.Basic (view)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.UTF8 as BU
import           Control.Exception         (Exception, catch, throwIO)
import           Network.HTTP.Client       (HttpException (StatusCodeException),
                                            Manager, httpLbs, parseUrl, newManager,
                                            responseBody, setQueryString, getUri)
import Network.HTTP.Types.Header (ResponseHeaders)



type Token = ByteString
type URL = String
type QueryString = [(ByteString, Maybe ByteString)]



newtype User = User
               {~
                id :: Int,
                firstName :: Text,
                lastName :: Maybe Text,
                username :: Maybe Text
               } deriving (Show)

parseUser v = do
    id <- v .: "id"
    firstName <- v .: "first_name"
    lastName <- v .:? "last_name"
    username <- v .:? "username"
    return
        (User
         {~ id = id
         , firstName = firstName
         , lastName = lastName
         , username = username
         })


instance FromJSON User where
  parseJSON (Object v) = parseUser v
  parseJSON _ = mzero


newtype GroupChat = GroupChat
                    {~
                     id :: Integer,
                     title :: Text
                    } deriving (Show)

parseGroupChat v = do
    id <- v .: "id"
    title <- v .: "title"
    return (GroupChat {~ id = id, title = title})

instance FromJSON GroupChat where
  parseJSON (Object v) = parseGroupChat v
  parseJSON _ = mzero

data Chat = Single User
          | Group GroupChat
          deriving (Show)

parseChat hasFirstName value
  | hasFirstName = Single <$> parseUser value
  | otherwise = Group <$> parseGroupChat value

instance FromJSON Chat where
  parseJSON (Object v) = parseValue
    where hasFirstName = member "first_name" v
          parseValue = parseChat hasFirstName v
  parseJSON _ = mzero

newtype PhotoSize = PhotoSize
                    {~ fileId :: Text
                    , width :: Int
                    , height :: Int
                    , fileSize :: Int
                    } deriving (Show)

parsePhotoSize v = do
    fileId <- v .: "file_id"
    width <- v .: "width"
    height <- v .: "height"
    fileSize <- v .: "file_size"
    return
        (PhotoSize
         {~ fileId = fileId
         , width = width
         , height = height
         , fileSize = fileSize
         })

instance FromJSON PhotoSize where
  parseJSON (Object v) = parsePhotoSize v
  parseJSON _ = mzero

type Photo = [PhotoSize]

newtype Audio = Audio
   {~ fileId :: Text
    , duration :: Int
    , mimeType :: Maybe Text
    , fileSize :: Maybe Int
    } deriving (Show)

parseAudio v = do
    fileId <- v .: "file_id"
    duration <- v .: "duration"
    mimeType <- v .:? "mime_type"
    fileSize <- v .:? "file_size"
    return
        (Audio
         {~ fileId = fileId
         , duration = duration
         , mimeType = mimeType
         , fileSize = fileSize
         })

instance FromJSON Audio where
  parseJSON (Object v) = parseAudio v
  parseJSON _ = mzero

newtype Document = Document
                   {~ fileId :: Text
                   , thumb :: PhotoSize
                   , fileName :: Maybe Text
                   , mimeType :: Maybe Text
                   , fileSize :: Maybe Int
                } deriving (Show)

parseDocument v = do
    fileId <- v .: "file_id"
    thumb <- v .: "thumb"
    fileName <- v .:? "file_name"
    mimeType <- v .:? "mime_type"
    fileSize <- v .:? "file_size"
    return
        (Document
         {~ fileId = fileId
         , thumb = thumb
         , fileName = fileName
         , mimeType = mimeType
         , fileSize = fileSize
         })

instance FromJSON Document where
  parseJSON (Object v) = parseDocument v
  parseJSON _ = mzero

newtype Sticker = Sticker
                  {~ fileId :: Text
                  , width :: Int
                  , height :: Int
                  , thumb :: PhotoSize
                  , fileSize :: Maybe Int
                  } deriving (Show)

parseSticker v = do
    fileId <- v .: "file_id"
    width <- v .: "width"
    height <- v .: "height"
    thumb <- v .: "thumb"
    fileSize <- v .:? "file_size"
    return
        (Sticker
         {~ fileId = fileId
         , width = width
         , height = height
         , thumb = thumb
         , fileSize = fileSize
         })

instance FromJSON Sticker where
  parseJSON (Object v) = parseSticker v
  parseJSON _ = mzero

newtype Video = Video
                {~ fileId :: Text
                , width :: Int
                , height :: Int
                , duration :: Int
                , thumb :: PhotoSize
                , mimeType :: Maybe Text
                , fileSize :: Maybe Int
                , caption :: Maybe Text
                } deriving (Show)

parseVideo v = do
    fileId <- v .: "file_id"
    width <- v .: "width"
    height <- v .: "height"
    duration <- v .: "duration"
    thumb <- v .: "thumb"
    mimeType <- v .:? "mime_type"
    fileSize <- v .:? "file_size"
    caption <- v .:? "caption"
    return
        (Video
         {~ fileId = fileId
         , width = width
         , height = height
         , duration = duration
         , thumb = thumb
         , mimeType = mimeType
         , fileSize = fileSize
         , caption = caption
         })

instance FromJSON Video where
  parseJSON (Object v) = parseVideo v
  parseJSON _ = mzero

newtype Contact = Contact
                  {~ phoneNumber :: Text
                  , firstName :: Text
                  , lastName :: Maybe Text
                  , userId :: Maybe Text
                  } deriving (Show)

parseContact v = do
    phoneNumber <- v .: "phone_number"
    firstName <- v .: "first_name"
    lastName <- v .:? "last_name"
    userId <- v .:? "user_id"
    return
        (Contact
         {~ phoneNumber = phoneNumber
         , firstName = firstName
         , lastName = lastName
         , userId = userId
         })

instance FromJSON Contact where
  parseJSON (Object v) = parseContact v
  parseJSON _ = mzero

newtype Location = Location
                   {~ longitude :: Double
                   , latitude :: Double
                   } deriving (Show)

parseLocation v = do
    longitude <- v .: "longitude"
    latitude <- v .: "latitude"
    return
        (Location
         {~ longitude = longitude
         , latitude = latitude
         })

instance FromJSON Location where
  parseJSON (Object v) = parseLocation v
  parseJSON _ = mzero

newtype UserProfilePhotos = UserProfilePhotos
                            {~ totalCount :: Int
                            , photos :: [Photo]
                            } deriving (Show)

parseUserProfilePhotos v = do
    totalCount <- v .: "total_count"
    photos <- v .: "photos"
    return
        (UserProfilePhotos
         {~ totalCount = totalCount
         , photos = photos
         })

instance FromJSON UserProfilePhotos where
  parseJSON (Object v) = parseUserProfilePhotos v
  parseJSON _ = mzero

newtype ReplyKeyboardMarkup = ReplyKeyboardMarkup
                              {~ keyboard :: [[Text]]
                              , resizeKeyboard :: Maybe Bool
                              , oneTimeKeyboard :: Maybe Bool
                              , selective :: Maybe Bool
                              } deriving (Show)

parseReplyKeyboardMarkup v = do
    keyboard <- v .: "keyboard"
    resizeKeyboard <- v .:? "resizeKeyboard"
    oneTimeKeyboard <- v .:? "one_time_keyboard"
    selective <- v .:? "selective"
    return
        (ReplyKeyboardMarkup
         {~ keyboard = keyboard
         , resizeKeyboard = resizeKeyboard
         , oneTimeKeyboard = oneTimeKeyboard
         , selective = selective
         })

instance FromJSON ReplyKeyboardMarkup where
  parseJSON (Object v) = parseReplyKeyboardMarkup v
  parseJSON _ = mzero

instance ToJSON ReplyKeyboardMarkup where
    toJSON (ReplyKeyboardMarkup rkm) = object
            [ "keyboard" .= (view @keyboard rkm)
            , "resize_keyboard" .= (view @resizeKeyboard rkm)
            , "one_time_keyboard" .= (view @oneTimeKeyboard rkm)
            , "selective" .= (view @selective rkm)]

newtype ReplyKeyboardHide = ReplyKeyboardHide
                            {~ hideKeyboard :: Bool
                            , selective :: Maybe Bool
                            } deriving (Show)

parseReplyKeyboardHide v = do
    hideKeyboard <- v .: "hide_keyboard"
    selective <- v .:? "selective"
    return
        (ReplyKeyboardHide
         {~ hideKeyboard = hideKeyboard
         , selective = selective
         })

instance FromJSON ReplyKeyboardHide where
  parseJSON (Object v) = parseReplyKeyboardHide v
  parseJSON _ = mzero

instance ToJSON ReplyKeyboardHide where
    toJSON (ReplyKeyboardHide rkh) = object
            [ "hide_keyboard" .= (view @hideKeyboard rkh)
            , "selective" .= (view @selective rkh)]

newtype ForceReply = ForceReply
                     {~ forceReply :: Bool
                     , selective :: Maybe Bool
                     } deriving (Show)

parseForceReply v = do
    forceReply <- v .: "force_reply"
    selective <- v .:? "selective"
    return
        (ForceReply
         {~ forceReply = forceReply
         , selective = selective
         })

instance FromJSON ForceReply where
  parseJSON (Object v) = parseForceReply v
  parseJSON _ = mzero

instance ToJSON ForceReply where
    toJSON (ForceReply fr) = object
            [ "forse_reply" .=
              (view @forceReply fr)
            , "selective" .=
              (view @selective fr)]

data Reply = Markup ReplyKeyboardMarkup
           | Hide ReplyKeyboardHide
           | Force ForceReply
           deriving (Show)

parseReply hasKeyboard hasHideKeyboard hasForceReply v
  | hasKeyboard = Markup <$> parseReplyKeyboardMarkup v
  | hasHideKeyboard = Hide <$> parseReplyKeyboardHide v
  | hasForceReply = Force <$> parseForceReply v

instance FromJSON Reply where
  parseJSON (Object v) = parseValue
    where hasKeyboard = member "keyboard" v
          hasHideKeyboard = member "hide_keyboard" v
          hasForceReply = member "force_reply" v
          parseValue  = parseReply hasKeyboard hasHideKeyboard hasForceReply v
  parseJSON _ = mzero

instance ToJSON Reply where
  toJSON (Markup m) = toJSON m
  toJSON (Hide h) = toJSON h
  toJSON (Force f) = toJSON f

newtype Message = Message
                  {~
                    messageId            :: Int
                  , from                 :: User
                  , date                 :: Int
                  , chat                 :: Chat
                  , forwardFrom          :: Maybe User
                  , forwardDate          :: Maybe Int
                  , replyToMessage       :: Maybe Message
                  , text                 :: Maybe Text
                  , audio                :: Maybe Audio
                  , document             :: Maybe Document
                  , photo                :: Maybe Photo
                  , sticker              :: Maybe Sticker
                  , video                :: Maybe Video
                  , contact              :: Maybe Contact
                  , location             :: Maybe Location
                  , newChatParticipant  :: Maybe User
                  , leftChatParticipant :: Maybe User
                  , newChatTitle         :: Maybe Text
                  , newChatPhoto         :: Maybe Photo
                  , deleteChatPhoto      :: Maybe Bool
                  , groupChatCreated     :: Maybe Bool
                  } deriving (Show)

parseMessage v = do
    messageId <- v .: "message_id"
    from <- v .: "from"
    date <- v .: "date"
    chat <- v .: "chat"
    forwardFrom <- v .:? "forward_from"
    forwardDate <- v .:? "forward_date"
    replyToMessage <- v .:? "reply_to_message"
    text <- v .:? "text"
    audio <- v .:? "audio"
    document <- v .:? "document"
    photo <- v .:? "photo"
    sticker <- v .:? "sticker"
    video <- v .:? "video"
    contact <- v .:? "contact"
    location <- v .:? "location"
    newChatParticipant <- v .:? "new_chat_participant"
    leftChatParticipant <- v .:? "left_chat_paricipant"
    newChatTitle <- v .:? "new_chat_title"
    newChatPhoto <- v .:? "new_chat_photo"
    deleteChatPhoto <- v .:? "delete_chat_photo"
    groupChatCreated <- v .:? "group_chat_created"
    return
        (Message
         {~ messageId = messageId
         , from = from
         , date = date
         , chat = chat
         , forwardFrom = forwardFrom
         , forwardDate = forwardDate
         , replyToMessage = replyToMessage
         , text = text
         , audio = audio
         , document = document
         , photo = photo
         , sticker = sticker
         , video = video
         , contact = contact
         , location = location
         , newChatParticipant = newChatParticipant
         , leftChatParticipant = leftChatParticipant
         , newChatTitle = newChatTitle
         , newChatPhoto = newChatPhoto
         , deleteChatPhoto = deleteChatPhoto
         , groupChatCreated = groupChatCreated
         })

instance FromJSON Message where
  parseJSON (Object v) = parseMessage v
  parseJSON _ = mzero


newtype Update = Update
                 {~ updateId :: Int
                 , message  :: Maybe Message
                 } deriving (Show)

parseUpdate v = do
    updateId <- v .: "update_id"
    message <- v .:? "message"
    return
        (Update
         {~ updateId = updateId
         , message = message
         })

instance FromJSON Update where
  parseJSON (Object v) = parseUpdate v
  parseJSON _ = mzero


type TelegramAPI a = ReaderT TelegramAPIConfig IO a

runTelegramAPI :: (MonadIO m) => TelegramAPIConfig -> TelegramAPI a -> m a
runTelegramAPI config action = liftIO (runReaderT action config)


getMe :: TelegramAPI User
getMe = telegramGetQuery "/getMe" []


getUpdates :: Maybe Int -> Maybe Int -> Maybe Int -> TelegramAPI [Update]
getUpdates offset limit timeout = telegramGetQuery
        "/getUpdates"
        (map
             (\(k,v) ->
                   (k, toQueryValue <$> v))
             [("offset", offset), ("limit", limit), ("timeout", timeout)])


sendMessage :: Int
            -> Text
            -> Maybe Bool
            -> Maybe Int
            -> Maybe Reply
            -> TelegramAPI Message
sendMessage chatId text disableWebPageReview replToMessageId replyMarkup = telegramGetQuery
        "/sendMessage"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("text", Just (encodeUtf8 text))
        , ("disable_web_page_review", toQueryValue <$> disableWebPageReview)
        , ("repl_to_message_id", toQueryValue <$> replToMessageId)
        , ("reply_markup", (BL.toStrict . encode) <$> replyMarkup)]


forwardMessage :: Int -> Int -> Int -> TelegramAPI Message
forwardMessage chatId fromChatId messageId = telegramGetQuery
        "/forwardMessage"
        (map
             (\(k,v) ->
                   (k, Just (toQueryValue v)))
             [ ("chat_id", chatId)
             , ("from_chat_id", fromChatId)
             , ("message_id", messageId)])


-- | Unfortunately we can not send a file, because i use for send
-- only GET method hence photo arg would be only String (in my imlementation Text)
sendPhoto :: Int
          -> Text
          -> Maybe Text
          -> Maybe Int
          -> Maybe Reply
          -> TelegramAPI Message
sendPhoto chatId photo caption replyToMessageId replyMarkup = telegramGetQuery
        "/sendPhoto"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("photo", Just (encodeUtf8 photo))
        , ("caption", encodeUtf8 <$> caption)
        , ("reply_to_message_id", toQueryValue <$> replyToMessageId)
        , ( "reply_markup"
          , (BL.toStrict . encode) <$>
            replyMarkup)]


sendAudio :: Int -> Text -> TelegramAPI Message
sendAudio chatId audio = telegramGetQuery
        "/sendAudio"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("audio", Just (encodeUtf8 audio))]


sendDocument :: Int -> Text -> Maybe Int -> Maybe Reply -> TelegramAPI Message
sendDocument chatId document replyToMessageId replyMarkup = telegramGetQuery
        "/sendDocument"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("document", Just (encodeUtf8 document))
        , ("reply_to_message_id", toQueryValue <$> replyToMessageId)
        , ( "reply_markup"
          , (BL.toStrict . encode) <$>
            replyMarkup)]


sendSticker :: Int -> Text -> TelegramAPI Message
sendSticker chatId sticker = telegramGetQuery
        "/sendSticker"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("sticker", Just (encodeUtf8 sticker))]


sendVideo :: Int -> Text -> Maybe Int -> Maybe Reply -> TelegramAPI Message
sendVideo chatId video replyToMessageId replyMarkup = telegramGetQuery
        "/sendVideo"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("video", Just (encodeUtf8 video))
        , ("reply_to_message_id", toQueryValue <$> replyToMessageId)
        , ( "reply_markup"
          , (BL.toStrict . encode) <$>
            replyMarkup)]


sendLocation :: Int
             -> Double
             -> Double
             -> Maybe Int
             -> Maybe Reply
             -> TelegramAPI Message
sendLocation chatId latitude longitude replyToMessageId replyMarkup = telegramGetQuery
        "/sendLocation"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("latitude", Just (toQueryValue latitude))
        , ("longitude", Just (toQueryValue longitude))
        , ("reply_to_message_id", toQueryValue <$> replyToMessageId)
        , ( "reply_markup"
          , (BL.toStrict . encode) <$>
            replyMarkup)]


sendChatAction :: Int -> Text -> TelegramAPI Message
sendChatAction chatId action = telegramGetQuery
        "/sendChatAction"
        [ ("chat_id", Just (toQueryValue chatId))
        , ("action", Just (encodeUtf8 action))]


getUserProfilePhotos :: Int
                     -> Maybe Int
                     -> Maybe Int
                     -> TelegramAPI UserProfilePhotos
getUserProfilePhotos userId offset limit = telegramGetQuery
        "/getUserProfilePhotos"
        [ ("user_id", Just (toQueryValue userId))
        , ("offset", toQueryValue <$> offset)
        , ("limit", toQueryValue <$> limit)]

-----------------------------------
----- Utils
-----------------------------------

type Command = ByteString

-- | Telegram API Response
data TelegramAPIR a
    = TelegramAPIR { result :: a}
    | TelegramAPIError { errorCode   :: Int
                       , description :: Maybe Text}
    deriving (Show)

instance (FromJSON a) => FromJSON (TelegramAPIR a) where
    parseJSON (Object v) = if parse (.: "ok") v ==
                              Success True
            then TelegramAPIR <$> v .: "result"
            else TelegramAPIError <$> v .: "error_code" <*> v .:? "description"
    parseJSON _ = mzero

-- | Telegram API Config
data TelegramAPIConfig = TelegramAPIConfig
    { telegramToken   :: Token
    , telegramManager :: Manager
    }


createUrl :: Token -> Command -> URL
createUrl token command = unpack
        (concat ["https://api.telegram.org/bot", token, command])


telegramGetQuery :: (FromJSON a)
                 => Command -> QueryString -> TelegramAPI a
telegramGetQuery command queryString = do
    config <- ask
    response <-
        liftIO $
        getQuery
            (telegramManager config)
            (createUrl (telegramToken config) command)
            queryString
    case decodeTelegramAPIResponse response of
        Just (TelegramAPIR res) -> return res
        _ -> liftIO mzero


decodeTelegramAPIResponse :: (FromJSON a)
                          => BL.ByteString -> Maybe (TelegramAPIR a)
decodeTelegramAPIResponse = decode


getQuery :: Manager -> URL -> QueryString -> IO BL.ByteString
getQuery manager url queryString = parseUrl url >>=
    \request ->
         catch
             (httpLbs (setQueryString queryString request) manager >>=
              \rb ->
                   return (responseBody rb))
             catchException
    where catchException :: HttpException -> IO BL.ByteString
          catchException e@(StatusCodeException _ rh _) = case extractResponseBody
                                                                   rh of
                  Just res -> return res
                  Nothing -> throwIO e
          catchException e = throwIO e
          extractResponseBody :: ResponseHeaders -> Maybe BL.ByteString
          extractResponseBody rheaders = BL.fromStrict `fmap`
              lookup "X-Response-Body-Start" rheaders


createTelegramConfig :: MonadIO m
                      => Token -> m TelegramAPIConfig
createTelegramConfig token = do
    man <- liftIO (newManager tlsManagerSettings)
    return
        TelegramAPIConfig
        { telegramToken = token
        , telegramManager = man
        }

toQueryValue :: (Show a) => a -> ByteString
toQueryValue = BU.fromString . show
