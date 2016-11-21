-- {-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Statsd where

import           Control.Monad (unless)
import           Control.Exception (try, throwIO)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified GHC.IO.Exception           as G
import           Network.Socket
import qualified Network.Socket.ByteString  as NSBS
import           Pipes

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await  -- 'await' a 'String'
    x   <- lift $ try $ putStrLn str
    case x of
        -- Gracefully terminate if we got a broken pipe error
        Left e@G.IOError { G.ioe_type = t} ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn

udpSink :: Socket -> Consumer (SockAddr, BS.ByteString) IO ()
udpSink udpSocket = do
  (addr, str) <- await
  x   <- lift $ try $ NSBS.sendTo udpSocket str addr
  case x of
      -- Gracefully terminate if we got a broken pipe error
      Left e@G.IOError { G.ioe_type = t} ->
          lift $ unless (t == G.ResourceVanished) $ throwIO e
      -- Otherwise loop
      Right _ -> udpSink udpSocket

addressedMessages :: SockAddr -> Producer (SockAddr, BS.ByteString) IO ()
addressedMessages sockAddr = each ((\i -> (sockAddr, BSC.pack ("Message " ++ show i ++ "\n"))) `fmap` [1..1000000 :: Int])
  -- yield (sockAddr, "Message 1\n")
  -- yield (sockAddr, "Message 2\n")

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "34567")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  runEffect $ addressedMessages (addrAddress serveraddr) >-> udpSink sock
  return ()
