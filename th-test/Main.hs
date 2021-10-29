{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Servant.ActiveResource
import Servant.Server.Generic (AsServerT)

data Dummy

instance (Monad m) => Resource Dummy m where
  type ResourceData Dummy = ()
  type StoredResourceData Dummy = ()

  listResources = pure []
  createResource _ = pure $ Right ()
  readResource _ = pure $ Right ()
  upsertResource _ _ = pure $ Right (Created, ())
  deleteResource _ = pure $ Right ()

routes :: (Monad m) => ResourceRoutes Dummy (AsServerT m)
routes = $(makeResourceServerT [t|Dummy|])

-- We only care that we compile.
main :: IO ()
main = pure ()
