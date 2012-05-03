module MPEG.Tools.PacketGenerator where

import MPEG.TS.AdaptationField
import MPEG.TS.TransportPacket
import qualified Data.ByteString.Lazy.Char8 as BC

emptyTransportPacket :: TransportPacket
emptyTransportPacket =
    TransportPacket { transportPacketFlags = [],
        pid = 0x10,
        scramblingControl = 0,
        continuityCounter = 0,
        adaptationField = Nothing,
        payload = Nothing }

exampleTransportPacket :: TransportPacket
exampleTransportPacket = packet
    where
        packet = TransportPacket { transportPacketFlags = [PayloadUnitStart, TransportPriority],
            pid = 0x10,
            scramblingControl = 0,
            continuityCounter = 0,
            adaptationField = Just af,
            payload = Just (BC.pack $ take 157 (repeat 'X')) }
        afe = AFE { ltw = Just 10,
            piecewiseRate = Just 1000,
            seamlessSplice = Just (SeamlessSplice 0 0x2100010029),
            extensionData = Nothing }
        af = AdaptationField { adaptationFieldFlags = [Discontinuity, RandomAccess, Priority],
            pcr = Just (PCR 0x500F0410004F 0),
            originalPcr = Just (PCR 0x500F0410004F 0),
            spliceCountdown = Just 10,
            privateData = Nothing,
            extension = Just afe,
            stuffingBytes = 0}