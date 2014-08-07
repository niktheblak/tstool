module MPEG.Tools.Analyzer(analyzeStream,
                            printPacketOrder,
                            printPackets) where

import Statistics
import MPEG.TS.TransportPacket
import MPEG.TS.FastTransportPacket
import MPEG.Tools.TransportBuffer
import MPEG.Tools.StreamIO
import Control.Exception(catch)
import Data.Binary
import Data.List(sort)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO
import System.IO.Error

process :: (Handle -> TransportBuffer -> IO ()) -> Handle -> FastTransportPacket -> TransportBuffer -> IO TransportBuffer
process f handle packet buf =
    let pid = getPid packet
        flags = getFlags packet
        newVideoFrame = elem PayloadUnitStart flags in
    if newVideoFrame
        then do
            let dist = calculateDistribution (M.elems buf)
            putStrLn (showTable dist)
            f handle buf
            return (M.delete pid buf)
        else return (insertPacket packet buf)
        
analyzeStream :: Handle -> FastTransportPacket -> TransportBuffer -> IO TransportBuffer
analyzeStream handle packet buf =
    let pid = getPid packet
        flags = getFlags packet
        newVideoFrame = elem PayloadUnitStart flags in
    if newVideoFrame
        then do
            {- let packets = concat (M.elems buf)
                pids = map (\p -> getPid p) packets
                dist = calculateDistribution (pids) -}
            let dist = packetDistribution buf
            putStrLn (showTable dist)
            return (insertPacket packet M.empty)
        else return (insertPacket packet buf)

printPackets :: Handle -> IO ()
printPackets handle = do
    packetData <- LB.hGet handle transportPacketLength
    let packet = decode packetData :: TransportPacket
    print packet
    eof <- hIsEOF handle
    if not eof
        then printPackets handle
        else return ()

printPacketOrder :: Handle -> IO ()
printPacketOrder handle =
    let next :: [Char] -> Char
        next [] = 'A'
        next keys = nextkey
            where
                nextkey = toEnum (fromEnum lastkey + 1)
                lastkey = last (sort keys)
        worker :: (M.Map Int Char) -> IO ()
        worker labelMap = catch
                (do packet <- readTransportPacket handle
                    let pid = getPid packet
                        --newMap = M.insertWith (\_ old -> next old) pid 'A' labelMap
                        (symbol, newMap) = if M.member pid labelMap
                            then (labelMap M.! pid, labelMap)
                            else (c, nm) where
                                c = nm M.! pid
                                nm = M.insert pid nk labelMap
                                nk = next (M.elems labelMap)
                    putChar symbol
                    if elem PayloadUnitStart (getFlags packet)
                        then putStrLn ""
                        else return ()
                    worker (newMap))
                (\e -> if isEOFError e
                    then putStrLn "" >> return ()
                    else ioError e)
    in
    worker M.empty
