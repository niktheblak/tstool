module MPEG.TS.TransportPacket(
    TransportPacket(..),
    TransportPacketFlags(..),
    Pid,
    Payload,
    transportPacketLength,
    headerLength,
    maxPayloadLength,
    syncByte) where

import StringUtils(indent)
import MPEG.TS.AdaptationField
import Control.Monad(when)
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

type Pid = Int
type Payload = LB.ByteString

-- | MPEG-2 Transport Stream packet, ISO/IEC 13818-1, page 18.
data TransportPacket = TransportPacket
    { -- | Flags, see 'TransportPacketFlags'.
      transportPacketFlags :: [TransportPacketFlags],
      -- | Program ID, indicates the type of the data stored in the packet
      -- payload.
      pid :: !Pid,
      -- | Indicates the scrambling mode of the Transport Stream packet payload.
      scramblingControl :: !Int,
      -- | The continuity_counter is a 4-bit field incrementing with each
      -- Transport Stream packet with the same PID.
      continuityCounter :: !Int,
      -- | Contains 'Some' 'AdaptationField' when this packet has an adaptation
      -- field, 'None' otherwise.
      adaptationField :: Maybe AdaptationField,
      -- | Contains the payload data as 'Some' 'ByteString' when this packet has
      -- a payload, 'None' otherwise.
      payload :: Maybe Payload }
    deriving Eq

-- | MPEG-2 Transport Stream packet flags, ISO/IEC 13818-1, page 19.
data TransportPacketFlags =
    -- | indicates that at least one uncorrectable bit error exists in the
    -- associated Transport Stream packet.
    TransportError |
    -- | Indicates that the payload of this Transport
    -- Stream packet will commence with the first byte of a PES packet. If this
    -- flag is not present, no PES packet shall start in this Transport Stream
    -- packet. If the PayloadUnitStart flag is present, then one and only one
    -- PES packet starts in this Transport Stream packet.
    PayloadUnitStart |
    -- | Indicates that the associated packet is of greater priority than other
    -- packets having the same PID which do not have the TransportPriority
    -- flag.
    TransportPriority
    deriving (Eq, Enum, Show)

instance Show TransportPacket where
    show tp =
        "[Transport Stream packet start]\n" ++
        "flags              : " ++ show (transportPacketFlags tp) ++ "\n" ++
        "pid                : " ++ show (pid tp) ++ "\n" ++
        "scrambling_control : " ++ show (scramblingControl tp) ++ "\n" ++
        "continuity_counter : " ++ show (continuityCounter tp) ++ "\n" ++
        "adaptation_field   : " ++ maybe "none" (\af -> "\n" ++ indent 4 (show af)) (adaptationField tp) ++ "\n" ++
        "payload            : " ++ maybe "none" (\pl -> show (LB.length pl) ++ " bytes") (payload tp)

instance Binary TransportPacket where
    put tp =
        let flagsAndPid = mkFlagsAndPid (transportPacketFlags tp) (pid tp)
            additionalFlags = mkAdditionalFlags tp
            payloadLength = maybe 0 adaptationFieldLength (adaptationField tp) + maybe 0 (fromIntegral . LB.length) (payload tp) 
            stuffing = transportPacketLength - headerLength - payloadLength
        in do
            when (payloadLength > maxPayloadLength) (fail "Too long payload")
            putWord8 syncByte
            putWord16be flagsAndPid
            putWord8 additionalFlags
            maybe (return ()) put (adaptationField tp)
            maybe (return ()) putLazyByteString (payload tp)
            when (stuffing > 0) (putByteString $ B.pack (take stuffing (repeat 0)))
    -- | Reads a transport packet from the stream.
    get = do
        sb <- getWord8
        when (sb /= syncByte) (fail ("Encountered an invalid sync byte while reading a transport packet: " ++ show sb))
        flagsAndPid <- getWord16be
        additionalFlags <- getWord8
        afield <- if hasAdaptationField additionalFlags
            then do
                af <- get :: Get AdaptationField
                return (Just af)
            else return Nothing
        pload <- if hasPayload additionalFlags
            then do
                let remainingData = maxPayloadLength -
                        maybe 0 adaptationFieldLength afield
                pldata <- getLazyByteString (fromIntegral remainingData)
                return (Just pldata)
            else return Nothing
        return TransportPacket { transportPacketFlags = getFlags flagsAndPid,
            pid = getPid flagsAndPid,
            scramblingControl = getScramblingControl additionalFlags,
            continuityCounter = getContinuityCounter additionalFlags,
            adaptationField = afield,
            payload = pload}

transportPacketLength :: Int
transportPacketLength = 188

headerLength :: Int
headerLength = 4

maxPayloadLength :: Int
maxPayloadLength = transportPacketLength - headerLength

pidMask :: Word16
pidMask = 0x1FFF

adaptationFieldControlMask :: Word8
adaptationFieldControlMask = 0x30

continuityCounterMask :: Word8
continuityCounterMask = 0xF

syncByte :: Word8
syncByte = 0x47

isTransportError :: Word16 -> Bool
isTransportError w = testBit w 15

isPayloadUnitStart :: Word16 -> Bool
isPayloadUnitStart w = testBit w 14

isTransportPriority :: Word16 -> Bool
isTransportPriority w = testBit w 13

mkFlagsAndPid :: [TransportPacketFlags] -> Pid -> Word16
mkFlagsAndPid flags pid = flags' .|. pid'
    where
        flags' = (if elem TransportError flags then bit 15 else 0) .|.
            (if elem PayloadUnitStart flags then bit 14 else 0) .|.
            (if elem TransportPriority flags then bit 13 else 0)
        pid' = (fromIntegral pid) .&. pidMask

mkAdditionalFlags :: TransportPacket -> Word8
mkAdditionalFlags tp = sc .|. afc .|. cc
    where
        sc = fromIntegral (scramblingControl tp) `shiftL` 6
        afc = ((maybe 0 (\_ -> bit 0) (adaptationField tp)) .|.
            (maybe 0 (\_ -> bit 1) (payload tp))) `shiftL` 4
        cc = fromIntegral (continuityCounter tp)

getPid :: Word16 -> Pid
getPid flagsAndPid = fromIntegral pid
    where pid = flagsAndPid .&. pidMask

getFlags :: Word16 -> [TransportPacketFlags]
getFlags flags =
    (if isTransportError flags then [TransportError] else []) ++
    (if isPayloadUnitStart flags then [PayloadUnitStart] else []) ++
    (if isTransportPriority flags then [TransportPriority] else [])

getScramblingControl :: Word8 -> Int
getScramblingControl flags = fromIntegral sc
    where sc = flags `shiftR` 6

getAdaptationFieldControl :: Word8 -> Int
getAdaptationFieldControl flags = fromIntegral afc
    where
        afc = afc' `shiftR` 4
        afc' = flags .&. adaptationFieldControlMask

getContinuityCounter :: Word8 -> Int
getContinuityCounter flags = fromIntegral cc
    where cc = flags .&. continuityCounterMask

hasAdaptationField :: Word8 -> Bool
hasAdaptationField b = testBit b 5

hasPayload :: Word8 -> Bool
hasPayload b = testBit b 4
