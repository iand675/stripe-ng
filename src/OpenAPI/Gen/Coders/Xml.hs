module OpenAPI.Gen.Coders.Xml
  ( httpEncodeXml
  , httpDecodeXml
  ) where

import Control.Effect
import Control.Effect.Error
import Data.ByteString (ByteString)
import OpenAPI.Support
import Xmlbf

httpEncodeXml :: ToXml a => a -> ByteString
httpEncodeXml = undefined

httpDecodeXml ::
     ( Monad m
     , Carrier sig m
     , Member (Error (HttpResponseError resp)) sig
     , FromXml resp
     )
  => HttpResponse
  -> m resp
httpDecodeXml = undefined
