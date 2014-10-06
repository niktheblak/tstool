module MPEG.TS.FastTransportPacket(Pid,
    FastTransportPacket,
    PacketContents(..),
    getPid,
    getFlags,
    getScramblingControl,
    getContinuityCounter,
    getPacketContents,
    readTransportPacket,
    writeTransportPacket) where

import MPEG.TS.AdaptationField
import MPEG.TS.TransportPacket
import Data.Bits
import Data.Binary(decode)
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

type FastTransportPacket = LB.ByteString

data PacketContents =
    Reserved |
    AdaptationFieldOnly AdaptationField |
    PayloadOnly Payload |
    Both (AdaptationField, Payload)
    deriving (Eq, Show)

pidMask :: Word16
pidMask = 0x1FFF

adaptationFieldControlMask :: Word8
adaptationFieldControlMask = 0x30

continuityCounterMask :: Word8
continuityCounterMask = 0xF

isTransportError :: Word16 -> Bool
isTransportError w = testBit w 15

isPayloadUnitStart :: Word16 -> Bool
isPayloadUnitStart w = testBit w 14

isTransportPriority :: Word16 -> Bool
isTransportPriority w = testBit w 13

showTransportPacket :: FastTransportPacket -> String
showTransportPacket tp =
    let tpd = decode tp :: TransportPacket
    in show tpd

getPid :: FastTransportPacket -> Pid
getPid packet =
    fromIntegral pid
    where
        pid = flagsAndPid .&. pidMask
        flagsAndPid = getFlagsAndPid packet

getFlags :: FastTransportPacket -> [TransportPacketFlags]
getFlags packet =
    let flags = getFlagsAndPid packet in
    (if isTransportError flags then [TransportError] else []) ++
    (if isPayloadUnitStart flags then [PayloadUnitStart] else []) ++
    (if isTransportPriority flags then [TransportPriority] else [])

getAdditionalFlags :: FastTransportPacket -> Word8
getAdditionalFlags packet = packet `LB.index` 3

getFlagsAndPid :: FastTransportPacket -> Word16
getFlagsAndPid packet =
    (b0 `shiftL` 8) .|. b1
    where
        b0 = fromIntegral (packet `LB.index` 1) :: Word16
        b1 = fromIntegral (packet `LB.index` 2) :: Word16

getScramblingControl :: FastTransportPacket -> Int
getScramblingControl packet = fromIntegral sc
    where
        sc = flags `shiftR` 6
        flags = getAdditionalFlags packet

getAFC :: FastTransportPacket -> Int
getAFC packet = fromIntegral afc
    where
        afc = afc' `shiftR` 4
        afc' = flags .&. adaptationFieldControlMask
        flags = getAdditionalFlags packet

getPacketContents :: FastTransportPacket -> PacketContents
getPacketContents packet =
    let afc = afc' `shiftR` 4
        afc' = flags .&. adaptationFieldControlMask
        flags = getAdditionalFlags packet
    in case afc of
        0 -> Reserved
        1 -> PayloadOnly (getPayload packet)
        2 -> AdaptationFieldOnly (getAdaptationField packet)
        3 -> Both (getBoth packet)
        n -> error ("Invalid adaptation field control: " ++ show n)

getContinuityCounter :: FastTransportPacket -> Int
getContinuityCounter packet = fromIntegral cc
    where
        cc = flags .&. continuityCounterMask
        flags = getAdditionalFlags packet

getAdaptationField :: FastTransportPacket -> AdaptationField
getAdaptationField tp =
    if hasAdaptationField tp
        then decode (LB.drop 4 tp)
        else error "Transport packet does not have adaptation field"

getPayload :: FastTransportPacket -> Payload
getPayload tp =
    if hasPayload tp
        then if hasAdaptationField tp
            then
                let af = getAdaptationField tp
                in (LB.drop (fromIntegral (headerLength + adaptationFieldLength af)) tp)
            else (LB.drop (fromIntegral headerLength) tp)
        else error "Transport packet does not have payload"

getBoth :: FastTransportPacket -> (AdaptationField, Payload)
getBoth tp = (af, pl)
    where
        af = getAdaptationField tp
        pl = LB.drop (fromIntegral (headerLength + adaptationFieldLength af)) tp 

hasAdaptationField :: FastTransportPacket -> Bool
hasAdaptationField tp = afc == 2 || afc == 3
    where afc = getAFC tp

hasPayload :: FastTransportPacket -> Bool
hasPayload tp = afc == 1 || afc == 3
    where afc = getAFC tp

readTransportPacket :: Handle -> IO FastTransportPacket
readTransportPacket handle = do
    sb' <- hGetChar handle
    let sb = fromIntegral (fromEnum sb')
    if sb /= syncByte
        then ioError $ userError ("Invalid sync byte " ++ show sb)
        else do
            packet <- LB.hGet handle (transportPacketLength - 1)
            if fromIntegral (LB.length packet) /= (transportPacketLength - 1)
                then ioError (userError "Could not read full 188 bytes.")
                else return (sb `LB.cons` packet)

writeTransportPacket :: Handle -> FastTransportPacket -> IO ()
writeTransportPacket handle packet
    | fromIntegral (LB.length packet) == transportPacketLength && LB.head packet == syncByte =
        LB.hPut handle packet
    | otherwise = ioError $ userError "Invalid packet length."
