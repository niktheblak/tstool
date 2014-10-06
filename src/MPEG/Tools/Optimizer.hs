module MPEG.Tools.Optimizer(optimizeStream) where

import MPEG.Tools.TransportBuffer
import MPEG.TS.TransportPacket(TransportPacketFlags(..))
import MPEG.TS.FastTransportPacket
import Data.List(sort)
import qualified Data.Map as M
import System.IO

ratios :: M.Map Pid Int -> M.Map Pid Int
ratios dist = M.map f dist
    where
        maxPackets = last $ sort (M.elems dist)
        f packetCount = ceiling r
            where r = fromIntegral maxPackets / fromIntegral packetCount

writeWithRatios' :: Handle -> TransportBuffer -> M.Map Pid Int -> IO ()
writeWithRatios' handle buf ratios =
    writePackets (concat $ M.elems buf) 1
    where
        writePackets :: [FastTransportPacket] -> Int -> IO ()
        writePackets [] _ = return ()
        writePackets packets counter =
            let processPacketsWithPid :: [FastTransportPacket] -> Int -> IO [FastTransportPacket]
                processPacketsWithPid [] _ = return []
                processPacketsWithPid (p : ps) n =
                    let pid = getPid p in
                    if n `mod` (ratios M.! pid) == 0
                        then do
                            writeTransportPacket handle p
                            processPacketsWithPid ps n
                        else do
                            rest <- processPacketsWithPid ps n
                            return (p : rest)
             in do
                remaining <- processPacketsWithPid packets counter
                writePackets remaining (counter + 1)

writeWithRatios :: Handle -> TransportBuffer -> M.Map Pid Int -> IO ()
writeWithRatios handle buffer ratios =
    let worker buf [] n = worker buf (M.keys ratios) n
        worker buf (pid : pids) n
            | M.null buf = return ()
            | otherwise = 
                if n `mod` (ratios M.! pid) == 0
                    then
                        let upd packets = if length packets > 1
                                then Just (tail packets)
                                else Nothing
                            newBuf = M.update upd pid buf
                            packets = buf M.! pid
                        in do
                            writeTransportPacket handle (head packets)
                            worker newBuf pids (n + 1)
                    else worker buf pids (n + 1)
    in worker buffer (M.keys ratios) 1

writeOut :: Handle -> TransportBuffer -> IO ()
writeOut handle buf =
    let writePacket = mapM_ (\p -> writeTransportPacket handle p) in
    mapM_ writePacket (M.elems buf)

optimizeStream :: Handle -> FastTransportPacket -> TransportBuffer -> IO TransportBuffer
optimizeStream handle packet buf =
    let pid = getPid packet
        flags = getFlags packet
        dist = packetDistribution buf
        newVideoFrame = elem PayloadUnitStart flags in
    if newVideoFrame
        then do
            writeOut handle buf
            return (insertPacket packet M.empty)
        else return (insertPacket packet buf)
