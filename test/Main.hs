{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Servant.ActiveResource
import Servant.Server.Generic (AsServer)

data Dummy

type instance ResourceData Dummy = ()

type instance StoredResourceData Dummy = ()

routes :: ResourceRoutes Dummy AsServer
routes =
  makeResourceRoutes
    ResourceOperations
      { listResources = pure [],
        createResource = const . pure $ Right (),
        readResource = const . pure $ Right (),
        upsertResource = const . const . pure $ Right (Created, ()),
        deleteResource = const . pure $ Right ()
      }

-- We only care that we compile.
main :: IO ()
main = pure ()
