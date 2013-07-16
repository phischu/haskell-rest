{-# LANGUAGE StandaloneDeriving, DeriveFunctor, OverloadedStrings #-}
module Web.Rest (
    Request(..),
    Method(..),Location,Accept,ContentType,Body,
    Response(..),
    ResponseCode,
    RestT,Rest,rest,
    runRestT,Hostname,Port,RestError(..)) where

import Web.Rest.Internal (
    Request(..),
    Method(..),Location,Accept,ContentType,Body,
    Response(..),
    ResponseCode,
    RestT,Rest,rest)

import Web.Rest.IOStreams (
    runRestT,Hostname,Port,RestError(..))
