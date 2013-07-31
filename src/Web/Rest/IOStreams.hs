{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
module Web.Rest.IOStreams (
    runRestT,Hostname,Port,RestError(..)) where

import Web.Rest.Internal (
    Request(..),Method(..),Response(Response),
    RestT,RestF(..))

import Control.Monad.Trans.Free (
    FreeT,FreeF(Pure,Free),runFreeT)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

import Control.Error (
    EitherT,fmapLT,runEitherT,scriptIO)

import Network.Http.Client (
    openConnection,closeConnection,Connection,
    buildRequest,http,setAccept,setContentType,
    sendRequest,inputStreamBody,
    receiveResponse,getStatusCode,getHeader,concatHandler)
import qualified Network.Http.Client (
    Method(..))
import System.IO.Streams.ByteString (fromByteString)

-- | The host url. For example "example.com".
type Hostname = Text

-- | The host port. For example 7474.
type Port     = Int

-- | Run a given description of REST calls against the given hostname, port and endpoint.
runRestT :: (MonadIO m) => Hostname -> Port -> RestT m a -> m (Either RestError a)
runRestT hostname port restt = runEitherT (do

        connection <- scriptIO (openConnection (encodeUtf8 hostname) (fromIntegral port))
            `onFailure` OpenConncetionError

        result     <- interpretRestT connection restt

        scriptIO (closeConnection connection)
            `onFailure` CloseConnectionError

        return result)

-- | Everything that might go wrong when running a rest monad with the io-streams backend.
data RestError = OpenConncetionError String
               | CloseConnectionError String
               | BuildRequestError String
               | ByteStringToStreamError String
               | SendRequestError String
               | ReceiveResponseError String

deriving instance Show RestError

-- | Run the specified rest calls against the given open connection reporting errors
--   in the 'EitherT' monad.
interpretRestT :: (MonadIO m) => Connection -> RestT m a -> EitherT RestError m a
interpretRestT connection restt = do
    next <- lift (runFreeT restt)
    case next of
        Pure result  -> return result
        Free (Rest request continue) -> (do

            httprequest <- scriptIO (buildRequest (do
                http (methodToMethod (method request)) (encodeUtf8 (location request))
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

            interpretRestT connection (continue response))

-- | Convert the package local definition of the http verb to the one used by io-streams.
methodToMethod :: Method -> Network.Http.Client.Method
methodToMethod GET    = Network.Http.Client.GET
methodToMethod POST   = Network.Http.Client.POST
methodToMethod PUT    = Network.Http.Client.PUT
methodToMethod DELETE = Network.Http.Client.DELETE

-- | Annotate an error.
onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT


