{-# LANGUAGE StandaloneDeriving, DeriveFunctor, OverloadedStrings #-}
module Web.Rest (
    Request(..),
    Method,Location,
    requestGET,
    Response(..),
    RestT,Rest,rest,
    Endpoint,
    runRestT,
    RestError(..)) where

import Control.Monad.Trans.Free (FreeT,FreeF(Pure,Free),liftF,runFreeT)
import Control.Monad.Trans (lift)

import Control.Monad.Identity (Identity)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,append)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.ByteString (ByteString)

import Control.Error (
    EitherT,fmapLT,runEitherT,scriptIO,left)
import Data.EitherR (catchT)

import Control.Exception (IOException)

import Network.Http.Client (
    Hostname,Port,openConnection,closeConnection,Connection,
    buildRequest,http,Method(GET,POST),setAccept,setContentType,
    sendRequest,inputStreamBody,
    receiveResponse,getStatusCode,getHeader,concatHandler)
import System.IO.Streams.ByteString (fromByteString)

-- | A typical Rest Request.
data Request = Request {
    method      :: Method,
    location    :: Location,
    accept      :: Text,
    requestType :: Text,
    requestBody :: ByteString
}

deriving instance Show Request

type Location = Text

requestGET :: Location -> Request
requestGET location = Request GET location "*/*" "*/*" ""

-- | A typical Rest Response.
data Response = Response {
    code         :: (Int,Int,Int),
    responseType :: Maybe Text,
    responseBody :: ByteString
}

deriving instance Show Response

data RestF a = Rest Request (Response -> a)

deriving instance Functor RestF

-- | The Rest Monad transformer. Add making REST calls to your stack of effects.
type RestT = FreeT RestF

-- | The Rest Monad.
type Rest  = RestT Identity

-- | Make a typical REST call.
rest :: (Monad m) => Request -> RestT m Response
rest request = liftF (Rest request id)

type Endpoint = Text

-- | Run a given description of REST calls against the given hostname, port and endpoint.
runRestT :: (MonadIO m) => Hostname -> Port -> Endpoint -> RestT m a -> m (Either RestError a)
runRestT hostname port endpoint restt = runEitherT (do
        connection <- scriptIO (openConnection hostname port) `onFailure` OpenConncetionError
        result <- interpretRestT connection endpoint restt
        scriptIO (closeConnection connection) `onFailure` CloseConnectionError
        return result)

interpretRestT :: (MonadIO m) => Connection -> Endpoint -> RestT m a -> EitherT RestError m a
interpretRestT connection endpoint restt = do
    next <- lift (runFreeT restt)
    case next of
        Pure result  -> return result
        Free (Rest request continue) -> (do

            httprequest <- scriptIO (buildRequest (do
                http (method request) (encodeUtf8 (endpoint `append` location request))
                setAccept (encodeUtf8 (accept request))
                setContentType (encodeUtf8 (requestType request))
                ))
                `onFailure` BuildRequestError

            bodyInputStream <- scriptIO (fromByteString (requestBody request))
                `onFailure` ByteStringToStreamError

            scriptIO (sendRequest connection httprequest (inputStreamBody bodyInputStream))
                `onFailure` SendRequestError

            response <- scriptIO (receiveResponse connection (\responseheader responsestream -> (do
                let code = getStatusCode responseheader
                    responsecode = (code `div` 100,(code `div` 10) `mod` 10,code `mod` 10)
                    responsetype = fmap decodeUtf8 (getHeader responseheader "Content-Type")
                responsebody <- concatHandler responseheader responsestream
                return (Response responsecode responsetype responsebody))))
                `onFailure` ReceiveResponseError

            interpretRestT connection endpoint (continue response))

data RestError = OpenConncetionError String
               | CloseConnectionError String
               | BuildRequestError String
               | ByteStringToStreamError String
               | SendRequestError String
               | ReceiveResponseError String

deriving instance Show RestError

-- | Annotate an error.
onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT

