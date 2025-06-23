{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
--
-- Module      : Servant.ActiveResource
-- Copyright   : (C) 2024-2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Types and helpers for defining Servant routes compatible with
-- [Rails's ActiveResource](https://github.com/rails/activeresource).
--
-- @
-- import qualified Servant.ActiveResource as AR
--
-- newtype MyResourceId = MyResourceId Int
-- -- Type for new values or updates to existing values. Usually
-- -- missing an @id@ field.
-- data MyResource = MyResource {...}
-- -- Like MyResource, but returned from the database.
-- data MyStoredResource = MyStoredResource {...}
--
-- -- These type family instances associate your resource's ID type
-- -- with the data types accepted and returned by your operations and
-- -- by the servant-server API.
-- type instance 'ResourceData' MyResourceId = MyResource
-- type instance 'StoredResourceData' MyResourceId = MyStoredResource
--
-- -- Record of routes, which can passed directly to
-- -- 'Servant.Server.Generic.genericServe', or spliced into another
-- -- record of routes via 'Servant.API.NamedRoutes'.
-- --
-- -- The exact monad used will depend on your program. Here, we just
-- -- assume 'Handler' from package servant-server.
-- routes :: AR.'ResourceRoutes' MyResourceId AsServer
-- routes = AR.'makeResourceRoutes' 'ResourceOperations'
--   { 'listResources' = ...
--   , 'createResource' = ...
--   , 'readResource' = ...
--   , 'upsertResource' = ...
--   , 'deleteResource' = ...
--   }
-- @
module Servant.ActiveResource
  ( -- * Declaring Resources
    ResourceData,
    StoredResourceData,
    ResourceOperations (..),

    -- ** Types used in CRUDL operation results
    NotFoundError (..),
    ValidationError (..),
    CreatedUpdated (..),

    -- ** Helpers
    hoistResourceOperations,

    -- * Types for @servant@ and @servant-server@
    ResourceRoutes (..),

    -- ** Creating 'ResourceRoutes'
    makeResourceRoutes,
    makeResourceRoutesT,

    -- * Miscellaneous
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
import Servant.API
  ( Capture,
    JSON,
    NoContent (..),
    ReqBody,
    StdMethod (..),
    UVerb,
    WithStatus (..),
    (:>),
  )
import Servant.API.ContentTypes (handleAcceptH)
import Servant.API.Generic ((:-))
import Servant.Server
  ( ErrorFormatters (..),
    Handler,
    defaultErrorFormatters,
    err422,
    errBody,
    errHeaders,
    getAcceptHeader,
    respond,
  )
import Servant.Server.Generic (AsServer, AsServerT)

-- | The type used to represent new records or updates to existing
-- records. Typically, this is missing an @id@ field.
--
-- @since 0.2.0.0
type family ResourceData id :: Type

-- | The type used to represent existing records.
--
-- @since 0.2.0.0
type family StoredResourceData id :: Type

-- | A 'Resource id m' record collects the implementations of CRUDL
-- handlers for a resource.
--
-- The type @id@ is the primary key for some abstract resource, and
-- operations on the collection of such resources all occur inside a
-- Monad @m@.
--
-- @since 0.2.0.0
data ResourceOperations id m = ResourceOperations
  { listResources ::
      m [StoredResourceData id],
    createResource ::
      ResourceData id ->
      m (Either ValidationError (StoredResourceData id)),
    readResource ::
      id ->
      m (Either NotFoundError (StoredResourceData id)),
    upsertResource ::
      id ->
      ResourceData id ->
      m (Either ValidationError (CreatedUpdated, StoredResourceData id)),
    deleteResource ::
      id ->
      m (Either NotFoundError ())
  }
  deriving stock (Generic)

-- | @since 0.1.0.0
data NotFoundError = NotFoundError
  deriving stock (Eq, Ord, Bounded, Show, Generic)

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
--
-- @since 0.1.0.0
newtype ValidationError = ValidationError (Map Text [Text])
  deriving stock (Eq, Show, Generic)

instance ToJSON ValidationError where
  toJSON (ValidationError errs) = object ["errors" .= toJSON errs]

-- | @since 0.1.0.0
data CreatedUpdated = Created | Updated
  deriving stock (Eq, Ord, Bounded, Show, Generic)

-- | Apply a natural tranformation to the CRUDL endpoints for a
-- resource.
--
-- @since 0.2.0.0
hoistResourceOperations ::
  (forall x. m x -> n x) -> ResourceOperations id m -> ResourceOperations id n
hoistResourceOperations nt ResourceOperations {..} =
  ResourceOperations
    { listResources = nt listResources,
      createResource = nt . createResource,
      readResource = nt . readResource,
      upsertResource = (nt .) . upsertResource,
      deleteResource = nt . deleteResource
    }

-- | The CRUDL routes required by an ActiveResource API, using Servant's
-- <https://docs.servant.dev/en/stable/cookbook/generic/Generic.html generic record-based routing>.
-- The easiest way to create one of these is to implement the CRUDL
-- operations in a 'ResourceOperations' record, and then apply
-- 'makeResourceRoutesT' or 'makeResourceRoutes' to it.
--
-- NOTE: If the
-- <https://github.com/haskell-servant/servant/issues/1466 alternate syntax>
-- for ':-' gets merged, we intend to adopt it; it's easier to read.
--
-- @since 0.1.0.0
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

-- | Turn a record of CRUDL operations operating in @servant-server@'s
-- 'Handler' monad into a record of endpoint handlers, suitable for
-- use with @servant-server@'s 'Servant.Server.Generic.NamedRoutes'.
--
-- Most of the time, you'll use 'makeResourceRoutesT', which works in
-- your application's monad.
--
-- @since 0.2.0.0
makeResourceRoutes ::
  ResourceOperations id Handler -> ResourceRoutes id AsServer
makeResourceRoutes = makeResourceRoutesT

-- | Turn a record of CRUDL operations into a actual record of
-- endpoint handlers. This works for any application monad.
--
-- @since 0.2.0.0
makeResourceRoutesT ::
  (Monad m) => ResourceOperations id m -> ResourceRoutes id (AsServerT m)
makeResourceRoutesT ResourceOperations {..} =
  ResourceRoutes
    { listRoute =
        listResources
          >>= respond . WithStatus @200,
      createRoute =
        createResource
          >=> either
            (respond . WithStatus @422)
            (respond . WithStatus @201),
      readRoute =
        readResource
          >=> either
            (respond . WithStatus @404)
            (respond . WithStatus @200),
      upsertRoute = \id_ data_ ->
        upsertResource id_ data_
          >>= either
            (respond . WithStatus @422)
            ( \case
                (Created, r) -> respond $ WithStatus @201 r
                (Updated, r) -> respond $ WithStatus @200 r
            ),
      deleteRoute =
        deleteResource
          >=> either
            (respond . WithStatus @404)
            (const (respond $ WithStatus @204 NoContent))
    }

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
--
-- @since 0.1.0.0
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
