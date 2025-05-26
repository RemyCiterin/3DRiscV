module Arbiter where

import Blarney
import Blarney.Option
import Data.Proxy

-- A basic arbiter interface, each client can
-- do a request, and it receive a grant parameter
-- to know if it own the shared data
data ArbiterClient = ArbiterClient {
    request :: Action (),
    grant :: Bit 1
  } deriving(Generic, Interface)

-- Return a static arbiter
makeStaticArbiter :: Int -> Module [ArbiterClient]
makeStaticArbiter size = do
  liftNat size $ \ (_ :: Proxy sz) -> do
    req :: [Wire (Bit 1)] <- replicateM size (makeWire 0)

    let requests :: Bit sz = fromBitList [r.val | r <- req]
    let grant :: Bit sz = firstHot requests .&. requests

    return [ArbiterClient {
      request = do r <== 1,
      grant = untypedAt i grant
    } | (r,i) <- zip req [0..]]

-- Return a fair arbiter
makeFairArbiter :: Int -> Module [ArbiterClient]
makeFairArbiter size = do
  liftNat size $ \ (_ :: Proxy sz) -> do
    req :: [Wire (Bit 1)] <- replicateM size (makeWire 0)

    let requests :: Bit sz = fromBitList [r.val | r <- req]
    let (hist,grant) = fairScheduler (delay 0 hist, requests)

    return [ArbiterClient {
      request = do r <== 1,
      grant = untypedAt i grant
    } | (r,i) <- zip req [0..]]
