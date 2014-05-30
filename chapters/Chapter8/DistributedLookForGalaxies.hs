{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Chapter8.DistributedLookForGalaxies where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Closure
  
data GalaxyMessage = LookForGalaxy ProcessId | GalaxyFound String
                   deriving (Typeable, Generic)
instance Binary GalaxyMessage

traveller :: Process ()
traveller = do LookForGalaxy master <- expect
               send master (GalaxyFound "Andromeda")
               
master :: [NodeId] -> Process ()
master nodes =
  do myPid <- getSelfPid
     mapM_ (\node -> do pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
     forever $ do GalaxyFound g <- expect
                  say $ "found galaxy: " ++ g
