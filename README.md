# servant-activeresource

[ActiveResource](https://github.com/rails/activeresource) is a Rails
library for representing resources from a RESTful API as Ruby objects,
with a similar interface to the Rails ActiveRecord ORM.

This library provides types and TH helpers for describing such APIs,
and for implementing Servant-style servers to provide them.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Servant.ActiveResource as AR

newtype MyResourceId = MyResourceId Int
-- Type for new values or updates to existing values. Usually
-- missing an `id` field.
data MyResource = MyResource {...}
-- Like MyResource, but returned from the database.
data MyStoredResource = MyStoredResource {...}

-- The exact monad used will depend on your program. Here, we just assume
-- `Handler` from package servant-server.
instance AR.Resource MyResourceId Handler where
  type ResourceData MyResourceId = MyResource
  type StoredResourceData MyResourceId = MyStoredResource

  -- These form the implementation of your API.
  listResources = ...
  createResource = ...
  readResource = ...
  upsertResource = ...
  deleteResource = ...

-- Record of routes, which can be spliced into a top-level handler
-- via Servant.API.NamedRoutes.
routes :: AR.ResourceRoutes MyResourceId (AsServerT Handler)
routes = $(AR.makeResourceServerT [t|MyResourceId|])
```
