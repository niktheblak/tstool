module MPEG.TS.TransportPacketSemantics(
    PidType(..),
    validPid,
    pidType,
    allowPcr) where

import MPEG.TS.TransportPacket(Pid)

data PidType = ProgramAssociationTable |
    ConditionalAccessTable |
    TransportStreamDescriptionTable |
    Reserved Int |
    GeneralPurpose Int |
    NullPacket

data ScramblingControl = Unscrambled | UserDefined Int

scramblingControl :: Int -> ScramblingControl
scramblingControl 0 = Unscrambled
scramblingControl n = UserDefined n

validPid :: Pid -> Bool
validPid p
    | p >= 0 && p <= 0x1FFF = True
    | otherwise = False

pidType :: Pid -> PidType
pidType p
    | p == 0 = ProgramAssociationTable
    | p == 1 = ConditionalAccessTable
    | p == 2 = TransportStreamDescriptionTable
    | p >= 0x3 && p <= 0xF = Reserved p
    | p >= 0x10 && p <= 0x1FFE = GeneralPurpose p
    | p == 0x1FFF = NullPacket
    | otherwise = undefined

allowPcr :: Pid -> Bool
allowPcr p = case pidType p of
    ProgramAssociationTable -> True
    ConditionalAccessTable -> True
    GeneralPurpose _ -> True
    otherwise -> False
