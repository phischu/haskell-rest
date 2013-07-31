module Web.Rest.HTTP (
    runRestT,Hostname,Port,RestError(..)
    ) where

import Web.Rest.Internal (
    RestT,RestF(Rest),Method(..),
    Request(..),Response(..))

import Network.HTTP (
    simpleHTTP,
    mkHeader,HeaderName(HdrAccept,HdrContentType),lookupHeader)

import qualified Network.HTTP as HTTP (
    Request(Request),RequestMethod(..),
    Response(rspCode,rspHeaders,rspBody))

import Network.URI (
    URI(URI),URIAuth(URIAuth))

import Network.Stream (ConnError)

import Control.Error (
    EitherT,runEitherT,scriptIO,
    hoistEither,fmapLT)

import Control.Monad.Trans.Free (FreeT,FreeF(Pure,Free),runFreeT)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text,unpack,pack)

-- | The host url. For example "example.com".
type Hostname = Text

-- | The host port. For example 7474.
type Port     = Int

-- | Possible errors when running a 'RestT' with the HTTP backend.
data RestError = SimpleHTTPError String
               | ConnError ConnError

-- | Run the given rest calls against the given hostname and port. Return a 'Left' on
--   error.
runRestT :: (MonadIO m) => Hostname -> Port -> RestT m a -> m (Either RestError a)
runRestT hostname port = runEitherT . interpretRestT hostname port

-- | Run the given rest calls against the given hostname and port and fail in the
--   'EitherT' monad.
interpretRestT :: (MonadIO m) => Hostname -> Port -> RestT m a -> EitherT RestError m a
interpretRestT hostname port restt = do
    next <- lift (runFreeT restt)
    case next of
        Pure result -> return result
        Free (Rest request continue) -> do

            let httprequest = HTTP.Request httpuri httpmethod httpheaders httpbody
                httpuri     = URI "http:" (Just uriauth) (unpack (location request)) "" ""
                uriauth     = URIAuth "" (unpack hostname) (show port)
                httpmethod  = (methodToMethod (method request))
                httpheaders = [
                    mkHeader HdrAccept      (unpack (accept      request)),
                    mkHeader HdrContentType (unpack (requestType request))]
                httpbody    = (requestBody request) 

            resultresponse <- scriptIO (simpleHTTP httprequest)
                `onFailure` SimpleHTTPError

            httpresponse <- hoistEither resultresponse
                `onFailure` ConnError

            let mayberesponsetype = lookupHeader HdrContentType (HTTP.rspHeaders httpresponse)
                response = Response
                    (HTTP.rspCode httpresponse)
                    (fmap pack mayberesponsetype)
                    (HTTP.rspBody httpresponse)

            interpretRestT hostname port (continue response)

-- | Convert the package local definition of the http verb to the one used by io-streams.
methodToMethod :: Method -> HTTP.RequestMethod
methodToMethod GET    = HTTP.GET
methodToMethod POST   = HTTP.POST
methodToMethod PUT    = HTTP.PUT
methodToMethod DELETE = HTTP.DELETE

-- | Annotate an error.
onFailure :: Monad m => EitherT a m r -> (a -> b) -> EitherT b m r
onFailure = flip fmapLT
