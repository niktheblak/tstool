module MPEG.Tools.TransportBuffer where

import Statistics
import MPEG.TS.TransportPacket
import MPEG.TS.FastTransportPacket
import qualified Data.Map as M

type TransportBuffer = M.Map Pid [FastTransportPacket]

packetDistribution :: TransportBuffer -> M.Map Pid Int
packetDistribution buf = dist where
    packets = concat (M.elems buf)
    pids = map (\p -> getPid p) packets
    dist = calculateDistribution (pids)

insertPacket :: FastTransportPacket -> TransportBuffer -> TransportBuffer
insertPacket packet =
    let pid = getPid packet in
    M.insertWith (\new old -> new ++ old) pid [packet]
