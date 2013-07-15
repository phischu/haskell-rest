{-# LANGUAGE StandaloneDeriving, DeriveFunctor, OverloadedStrings #-}
module Web.Rest (
    Request(..),
    Method(..),Location,Accept,ContentType,Body,
    Response(..),
    RestT,Rest,rest,
    runRestT,RestError(..)) where

import Web.Rest.Internal (
    Request(..),
    Method(..),Location,Accept,ContentType,Body,
    Response(..),
    RestT,Rest,rest)

import Web.Rest.IOStreams (
    runRestT,RestError(..))
