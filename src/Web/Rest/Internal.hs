{-# LANGUAGE StandaloneDeriving, DeriveFunctor, OverloadedStrings #-}
module Web.Rest.Internal where

import Control.Monad.Trans.Free (FreeT,FreeF(Pure,Free),liftF,runFreeT)
import Control.Monad.Trans (lift)

import Control.Monad.Identity (Identity)

import Data.Text (Text)
import Data.ByteString (ByteString)

-- | The http verb.
data Method      = GET | POST | PUT | DELETE

deriving instance Show Method

-- | The target.
type Location     = Text

-- | The expected result type.
type Accept       = Text

-- | The type of the body.
type ContentType  = Text

-- | The content.
type Body         = ByteString

-- | The Response code.
type ResponseCode = (Int,Int,Int)

-- | A typical Rest Request.
data Request = Request {
    method      :: Method,
    location    :: Location,
    accept      :: Accept,
    requestType :: ContentType,
    requestBody :: Body
}

deriving instance Show Request

-- | A typical Rest Response.
data Response = Response {
    code         :: ResponseCode,
    responseType :: Maybe ContentType,
    responseBody :: Body
}

deriving instance Show Response

-- | The 'Functor' for rest calls underlying the free monad.
data RestF a = Rest Request (Response -> a)

deriving instance Functor RestF

-- | The Rest Monad transformer. Add making REST calls to your stack of effects.
type RestT = FreeT RestF

-- | The Rest Monad.
type Rest  = RestT Identity

-- | Make a typical REST call.
rest :: (Monad m) => Request -> RestT m Response
rest request = liftF (Rest request id)

