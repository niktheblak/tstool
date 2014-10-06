module MPEG.Tools.StreamIO(TransportBuffer,
                            readPackets,
                            processFile) where

import MPEG.TS.TransportPacket
import MPEG.TS.FastTransportPacket
import MPEG.Tools.TransportBuffer
import Control.Exception(catch)
import System.IO
import System.IO.Error
import qualified Data.Map as M

readPackets :: Handle -> IO [FastTransportPacket]
readPackets handle = catch
    (do packet <- readTransportPacket handle
        rest <- readPackets handle
        return (packet : rest))
    (\e -> (do putStrLn ("Error: " ++ show (e :: IOError) ++ ". Skipping packet...")
               seekToNextSyncByte handle
               rest <- readPackets handle
               return rest))

seekToNextSyncByte :: Handle -> IO ()
seekToNextSyncByte handle = do
    b <- hGetChar handle
    if fromEnum b /= fromIntegral syncByte
        then seekToNextSyncByte handle
        else return ()

processFile :: Handle -> (Handle -> FastTransportPacket -> TransportBuffer -> IO TransportBuffer) -> IO ()
processFile handle f =
    let worker :: TransportBuffer -> IO ()
        worker currentBuffer = catch
            (do packet <- readTransportPacket handle
                newBuffer <- f handle packet currentBuffer
                worker newBuffer)
            (\e -> if isUserError e
                then do
                    putStrLn ("Error: " ++ show e ++ ". Skipping packet...")
                    seekToNextSyncByte handle
                    worker currentBuffer
                else if isEOFError e
                    then return ()
                else ioError e)
     in
     worker M.empty
