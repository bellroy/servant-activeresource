{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


-- |
--
-- Module      : Servant.ActiveResource
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Types and helpers for defining Servant routes compatible with
-- [Rails' ActiveResource](https://github.com/rails/activeresource).
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
--
-- import qualified Servant.ActiveResource as AR
--
-- newtype MyResourceId = MyResourceId Int
-- -- Type for new values or updates to existing values. Usually
-- -- missing an @id@ field.
-- data MyResource = MyResource {...}
-- -- Like MyResource, but returned from the database.
-- data MyStoredResource = MyStoredResource {...}
--
-- -- The exact monad used will depend on your program. Here, we just assume
-- -- 'Handler' from package servant-server.
-- instance AR.'Resource' MyResourceId Handler where
--   type 'ResourceData' MyResourceId = MyResource
--   type 'StoredResourceData' MyResourceId = MyStoredResource
--
--   -- These form the implementation of your API.
--   'listResources' = ...
--   'createResource' = ...
--   'readResource' = ...
--   'upsertResource' = ...
--   'deleteResource' = ...
--
-- -- Record of routes, which can be spliced into a top-level handler
-- -- via 'Servant.API.NamedRoutes'.
-- routes :: AR.'ResourceRoutes' MyResourceId (AsServerT Handler)
-- routes = $(AR.'makeResourceServerT' [t|MyResourceId|])
-- @
module Servant.ActiveResource
  ( Resource (..),
    NotFoundError (..),
    ValidationError (..),
    CreatedUpdated (..),
    ResourceRoutes (..),
    makeResourceServerT,
    errorFormatters,
  )
where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Language.Haskell.TH as TH
import Servant.API
  ( Capture,
    JSON,
    NoContent (..), -- ctor import needed by TH
    ReqBody,
    StdMethod (..),
    UVerb,
    WithStatus (..), -- ctor import needed by TH
    (:>),
  )
import Servant.API.ContentTypes (handleAcceptH)
import Servant.API.Generic ((:-))
import Servant.Server
  ( ErrorFormatters (..),
    defaultErrorFormatters,
    err422,
    errBody,
    errHeaders,
    getAcceptHeader,
    respond, -- needed by TH
  )

-- | A 'Resource' instance says that the type @id@ is the primary
-- key for some abstract resource, and that the collection of such
-- resources can be manipulated with CRUDL actions inside a Monad @m@.
class (Monad m) => Resource id m where
  -- | The type used to represent new records or updates to existing
  -- records. Typically, this is missing an @id@ field.
  type ResourceData id :: Type

  -- | The type used to represent existing records.
  type StoredResourceData id :: Type

  listResources ::
    m [StoredResourceData id]
  createResource ::
    ResourceData id ->
    m (Either ValidationError (StoredResourceData id))
  readResource ::
    id ->
    m (Either NotFoundError (StoredResourceData id))
  upsertResource ::
    id ->
    ResourceData id ->
    m (Either ValidationError (CreatedUpdated, StoredResourceData id))
  deleteResource ::
    id ->
    m (Either NotFoundError ())

data NotFoundError = NotFoundError
  deriving (Eq, Ord, Bounded, Show, Generic)

instance ToJSON NotFoundError where
  toJSON NotFoundError =
    object
      [ "errors" .= object ["id" .= String "Not found"]
      ]

-- | ActiveResource errors are a map where the keys name fields from
-- the payload, and the values are lists of errors at that field.
--
-- The 'ToJSON' instance wraps the whole thing in an @{ "errors": ... }@
-- object, to match [ActiveResource's expectations](https://github.com/rails/activeresource/blob/3cf44f731f655dccc13ba23f78603f7e214e3352/lib/active_resource/validations.rb#L35-L71).
newtype ValidationError = ValidationError (Map Text [Text])
  deriving stock (Eq, Show, Generic)

instance ToJSON ValidationError where
  toJSON (ValidationError errs) = object ["errors" .= toJSON errs]

data CreatedUpdated = Created | Updated
  deriving (Eq, Ord, Bounded, Show, Generic)

-- | The CRUDL routes required by an ActiveResource API, using Servant's
-- <https://docs.servant.dev/en/stable/cookbook/generic/Generic.html generic record-based routing>.
-- Once you have a 'Resource' instance, use 'makeResourceServerT' to
-- generate a server that fills out this structure.
--
-- NOTE: If the
-- <https://github.com/haskell-servant/servant/issues/1466 alternate syntax>
-- for ':-' gets merged, we intend to adopt it; it's easier to read.
data ResourceRoutes (id :: Type) mode = ResourceRoutes
  { listRoute ::
      mode
        :- UVerb 'GET '[JSON] '[WithStatus 200 [StoredResourceData id]],
    createRoute ::
      mode
        :- ( ReqBody '[JSON] (ResourceData id)
               :> UVerb
                    'POST
                    '[JSON]
                    '[ WithStatus 422 ValidationError,
                       WithStatus 201 (StoredResourceData id)
                     ]
           ),
    readRoute ::
      mode
        :- ( Capture "id" id
               :> UVerb
                    'GET
                    '[JSON]
                    '[ WithStatus 404 NotFoundError,
                       WithStatus 200 (StoredResourceData id)
                     ]
           ),
    upsertRoute ::
      mode
        :- ( Capture "id" id
               :> ReqBody '[JSON] (ResourceData id)
               :> UVerb
                    'PUT
                    '[JSON]
                    '[ WithStatus 422 ValidationError,
                       WithStatus 200 (StoredResourceData id),
                       WithStatus 201 (StoredResourceData id)
                     ]
           ),
    deleteRoute ::
      mode
        :- ( Capture "id" id
               :> UVerb
                    'DELETE
                    '[JSON]
                    '[ WithStatus 404 NotFoundError,
                       WithStatus 204 NoContent
                     ]
           )
  }
  deriving (Generic)

-- | Given an instance of 'ResourceId ty', plumb through its operations to
-- build a 'ResourceRoutes'. GHC's type checker
-- <https://github.com/haskell-servant/servant/issues/1381 isn't smart enough>
-- to reason through the generated code if it's given a polymorphic
-- type, so we have to use TH.
--
-- If @m ~ ResourceMonad ty@, then you can call it like this:
--
-- @
-- myResourceServer :: 'ResourceRoutes' ty ('Servant.Generic.AsServerT' m)
-- myResourceServer = \$(makeResourceServerT [t|ty|])
-- @
--
-- The generated code will
-- need @-XDataKinds@, @-XLambdaCase@, and @-XTypeApplications@.
--
-- You can then serve it using the functions in "Servant.Server.Generic".
makeResourceServerT :: TH.TypeQ -> TH.ExpQ
makeResourceServerT ty =
  [|
    ResourceRoutes
      { listRoute =
          $(TH.appTypeE [|listResources|] ty)
            >>= $(respondWithStatus 200),
        createRoute =
          $(TH.appTypeE [|createResource|] ty)
            >=> either $(respondWithStatus 422) $(respondWithStatus 201),
        readRoute =
          $(TH.appTypeE [|readResource|] ty)
            >=> either $(respondWithStatus 404) $(respondWithStatus 200),
        upsertRoute = \id_ data_ ->
          $(TH.appTypeE [|upsertResource|] ty) id_ data_
            >>= either
              $(respondWithStatus 422)
              ( \case
                  (Created, r) -> $(respondWithStatus 201) r
                  (Updated, r) -> $(respondWithStatus 200) r
              ),
        deleteRoute =
          $(TH.appTypeE [|deleteResource|] ty)
            >=> either
              $(respondWithStatus 404)
              (const ($(respondWithStatus 204) NoContent))
      }
    |]
  where
    -- respondWithStatus n ===> respond . WithStatus @n
    respondWithStatus :: Integer -> TH.ExpQ
    respondWithStatus n = [|respond . $(TH.appTypeE [|WithStatus|] (TH.litT (TH.numTyLit n)))|]

-- | This is a custom error formatter for parse errors, which runs when
-- Servant fails to deserialise a request parameter (i.e., before endpoint
-- code runs).
--
-- Technically, such a parse failure should return
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/400 HTTP 400 Bad Request>,
-- but ActiveResource shows no useful information in such
-- cases. This formatter therefore gives up technical purity in favour of
-- reasonable error messages.
--
-- You can use it with @servant-server@'s various @...WithContext@ functions:
--
-- @
-- -- Supposing our 'ResourceMonad' is servant-server's 'Handler' type, so we
-- -- can pass 'id' as the natural transformation parameter:
-- 'Servant.Server.Generic.genericServeTWithContext' id myResourceServer (errorFormatters ':.' 'EmptyContext')
-- @
errorFormatters :: ErrorFormatters
errorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = \_ req err ->
        let value = object ["id" .= [err]]
            acceptHeader = getAcceptHeader req
         in case handleAcceptH (Proxy @'[JSON]) acceptHeader value of
              Nothing -> err422 {errBody = BL8.pack err}
              Just (contentType, body) ->
                err422
                  { errBody = body,
                    errHeaders = [("Content-Type", BL.toStrict contentType)]
                  }
    }
