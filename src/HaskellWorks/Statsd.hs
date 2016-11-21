{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Statsd where

import           Network.Socket
import qualified Network.Socket.ByteString as NSBS

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "34567")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  _ <- NSBS.sendTo sock "omsg" (addrAddress serveraddr)
  return ()
