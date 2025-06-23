# Revision history for servant-activeresource

## 0.2.0.0 -- 2025-06-24

* `ResourceData` and `StoredResourceData` are now standalone type
  families. This is valuable if you want to split the definition of
  your API types from their implementation, for example if you're
  using [`servant-openapi3`](https://hackage.haskell.org/package/servant-openapi3)
  to generate OpenAPI documentation for your API, and you don't want
  to recompile the API modules every time you tweak an endpoint
  implementation.

* The `Resource` typeclass no longer exists. Instead, a Haskell record
  `ResourceOperations` has been added, which holds the CRUDL
  operations on a resource.

  - Added `hoistResourceOperations` to apply a natural transformation
    to a `ResourceOperations`.

* There is no longer any need for Template
  Haskell. `makeResourceServerT` has been replaced with
  `makeResourceRoutesT`, which is a non-TH function that operates on
  `ResourceOperations`.

* Added `makeResourceRoutes`, a version of `makeResourceRoutesT`
  specialised to `Handler`.

## 0.1.0.0 -- 2024-07-05

* First version. Released on an unsuspecting world.
