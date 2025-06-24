# servant-activeresource

[ActiveResource](https://github.com/rails/activeresource) is a Rails
library for representing resources from a RESTful API as Ruby objects,
with a similar interface to the Rails ActiveRecord ORM.

This library provides types for describing such APIs, and functions to
help implement Servant-style servers that provide them.

```haskell
import qualified Servant.ActiveResource as AR

newtype MyResourceId = MyResourceId Int
-- Type for new values or updates to existing values. Usually
-- missing an `id` field.
data MyResource = MyResource {...}
-- Like MyResource, but returned from the database.
data MyStoredResource = MyStoredResource {...}

-- These type family instances associate your resource's ID type
-- with the data types accepted and returned by your operations and
-- by the servant-server API.
type instance ResourceData MyResourceId = MyResource
type instance StoredResourceData MyResourceId = MyStoredResource

-- Record of routes, which can passed directly to
-- 'Servant.Server.Generic.genericServe', or spliced into another
-- record of routes via 'Servant.API.NamedRoutes'.
--
-- The exact monad used will depend on your program. Here, we just
-- assume 'Handler' from package servant-server.
routes :: AR.ResourceRoutes MyResourceId AsServer
routes = AR.makeResourceRoutes ResourceOperations
  { listResources = ...
  , createResource = ...
  , readResource = ...
  , upsertResource = ...
  , deleteResource = ...
  }
```
