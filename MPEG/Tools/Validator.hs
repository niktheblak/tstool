module MPEG.Tools.Validator where

import MPEG.TS.TransportPacket
import MPEG.TS.AdaptationField
import MPEG.TS.TransportPacketSemantics

type ValidationResult = Either [String] TransportPacket

validateTransportPacket :: TransportPacket -> ValidationResult
validateTransportPacket tp = case adaptationField tp of
    Just af -> case pcr af of
        Just _ -> if allowPcr (pid tp) then success else Left ["PCR is not allowed with PID " ++ show (pid tp)]
        Nothing -> success
    Nothing -> success
    where
        success = Right tp
