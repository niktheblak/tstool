module MPEG.TS.AdaptationField where

import StringUtils(indent)
import Control.Monad(when)
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Debug.Trace
import System.IO
import qualified Data.ByteString.Lazy as LB

-- | Program Clock Reference. Indicates the intended time of arrival of
-- the byte containing the last bit of the program_clock_reference_base
-- at the input of the system target decoder. ISO/IEC 13818-1 page 23.
data PCR =
    -- | Type constructor for PCR. The first integer is PCR base and the second
    -- PCR extension. 
    PCR !Integer !Int deriving Eq

-- | Represents the optional MPEG-2 Transport Stream adaptation
-- field.
--
-- The adaptation field is described in ISO/IEC 13818-1, Table 2-6: Transport
-- Stream adaptation field, page 22.
data AdaptationField =
    AdaptationField {
        adaptationFieldFlags :: [AdaptationFieldFlags],
        -- | Program clock reference, see 'PCR'.
        pcr :: Maybe PCR,
        -- | Original program clock reference. Assists in the reconstruction of a
        -- single program Transport Stream from another Transport Stream.
        -- ISO/IEC 13818-1 page 23. See 'PCR'.
        originalPcr :: Maybe PCR,
        -- | Splice countdown, ISO/IEC 13818-1 page 24.
        spliceCountdown :: Maybe Int,
        -- | User-specific private data.
        privateData :: Maybe LB.ByteString,
        -- | Optional adaptation field extension, see
        -- 'AdaptationFieldExtension'.
        extension :: Maybe AdaptationFieldExtension,
        -- | Stuffing (padding) bytes. If a Transport Stream packet does not
        -- contain full 188 bytes of data, the missing bytes shall be filled
        -- with values 0xFF.
        stuffingBytes :: Int}
    deriving Eq

-- | Adaptation field flags. Consult the ISO/IEC 13818-1 page 23 for the
-- detailed explanations of these flags.
data AdaptationFieldFlags =
    -- | Indicates that the discontinuity state is true for the current
    -- Transport Stream packet.
    Discontinuity |
    -- | indicates that the current Transport Stream packet, and possibly
    -- subsequent Transport Stream packets with the same PID, contain some
    -- information to aid random access at this point.
    RandomAccess |
    -- | Indicates that the payload has a higher priority than the payloads of
    -- other Transport Stream packets.
    Priority
    deriving (Eq, Show, Enum)
    
-- | Seamless splice type. See ISO/IEC 13181-1 pages 26-30.
type SpliceType = Int

data SeamlessSplice = SeamlessSplice !SpliceType !Integer deriving Eq

-- | Represents the optional MPEG-2 Transport Stream adaptation field
-- extension.
--
-- The adaptation field extension is described in ISO/IEC 13818-1, Table 2-6:
-- Transport Stream adaptation field, page 22.
data AdaptationFieldExtension =
    AFE { ltw :: Maybe Int,
          piecewiseRate :: Maybe Int,
          seamlessSplice :: Maybe SeamlessSplice,
          extensionData :: Maybe LB.ByteString }
    deriving Eq
    
-- | Gets the length of an adaptation field in bytes.
adaptationFieldLength :: AdaptationField -> Int
adaptationFieldLength af = 2 +
    maybe 0 (\_ -> 6) (pcr af) +
    maybe 0 (\_ -> 6) (originalPcr af) +
    maybe 0 (\_ -> 1) (spliceCountdown af) +
    maybe 0 (\pd -> 1 + fromIntegral (LB.length pd)) (privateData af) +
    maybe 0 (\e -> adaptationFieldExtensionLength e) (extension af) +
    stuffingBytes af

-- | Gets the length of an adaptation field extension in bytes.
adaptationFieldExtensionLength :: AdaptationFieldExtension -> Int
adaptationFieldExtensionLength afe = 2 +
    maybe 0 (\_ -> 2) (ltw afe) +
    maybe 0 (\_ -> 3) (piecewiseRate afe) +
    maybe 0 (\_ -> 5) (seamlessSplice afe) +
    maybe 0 (\e -> fromIntegral (LB.length e)) (extensionData afe)
    
instance Show SeamlessSplice where
    show (SeamlessSplice spliceType dtsNextAu) = "type: " ++ show spliceType ++ " DTS_next_AU: " ++ show dtsNextAu
    
instance Show PCR where
    show (PCR base ext) = "base: " ++ show base ++ " extension: " ++ show ext

instance Show AdaptationField where
    -- | Generates a human-readable representation of the MPEG-2 TS adaptation
    -- field
    --
    -- The output format is similar to the description of the adaptation field
    -- in ISO/IEC 13818-1.
    show af =
        "[Adaptation Field start]\n" ++
        "length           : " ++ show (adaptationFieldLength af) ++ "\n" ++
        "flags            : " ++ show (adaptationFieldFlags af) ++ "\n" ++
        "pcr              : " ++ maybe "none" show (pcr af) ++ "\n" ++
        "original_pcr     : " ++ maybe "none" show (originalPcr af) ++ "\n" ++
        "splice_countdown : " ++ maybe "none" show (spliceCountdown af) ++ "\n" ++
        "private_data     : " ++ maybe "none" (\pd -> show (LB.length pd) ++ " bytes") (privateData af) ++ "\n" ++
        "extension        : " ++ maybe "none" (\e -> "\n" ++ indent 4 (show e)) (extension af) ++ "\n" ++
        "stuffing_bytes   : " ++ show (stuffingBytes af)
    
-- | 'AdaptationField' is made an instance of 'Binary' so it can be
-- serialized and deserialized into 'ByteString' streams.
--
-- The serialization format follows the MPEG-2 TS adaptation field
-- specifications in ISO/IEC 13818-1, Table 2-6: Transport Stream adaptation
-- field, page 22.
instance Binary AdaptationField where
    -- | Writes an adaptation field to the stream.
    put af =
        let afflags = adaptationFieldFlags af
            flags = (if elem Discontinuity afflags then bit 7 else 0) .|.
                (if elem RandomAccess afflags then bit 6 else 0) .|.
                (if elem Priority afflags then bit 5 else 0) .|.
                (maybe 0 (\_ -> bit 4) (pcr af)) .|.
                (maybe 0 (\_ -> bit 3) (originalPcr af)) .|.
                (maybe 0 (\_ -> bit 2) (spliceCountdown af)) .|.
                (maybe 0 (\_ -> bit 1) (privateData af)) .|.
                (maybe 0 (\_ -> bit 0) (extension af))
            len = adaptationFieldLength af
            putPCR pcr = let (high, low) = serializePcr pcr in do
                    putWord32be high
                    putWord16be low
        in do
            putWord8 (fromIntegral len)
            putWord8 flags
            maybe (return ()) putPCR (pcr af)
            maybe (return ()) putPCR (originalPcr af)
            maybe (return ()) (\sc -> putWord8 (fromIntegral sc)) (spliceCountdown af)
            maybe (return ()) putLazyByteString (privateData af)
            maybe (return ()) put (extension af)
            putLazyByteString $ LB.pack (take (stuffingBytes af) (repeat 0xFF))
    -- | Reads an adaptation field from the stream.
    get =
        -- Reads PCR or OPCR data from the stream.
        let getPCR = do
                high <- getWord32be
                low <- getWord16be
                return (deserializePcr (high, low))
        in do
            aflength <- getWord8
            iflags <- getWord8
            let isDiscontinuity = testBit iflags 7
                isRandomAccess = testBit iflags 6
                isPriority = testBit iflags 5
                hasPcr = testBit iflags 4
                hasOriginalPcr = testBit iflags 3
                isSplicingPoint = testBit iflags 2
                hasPrivateData = testBit iflags 1
                hasExtension = testBit iflags 0
                flags = (if isDiscontinuity then [Discontinuity] else []) ++
                        (if isRandomAccess then [RandomAccess] else []) ++
                        (if isPriority then [Priority] else [])
            pcr <- if hasPcr
                then do
                    pcrData <- getPCR
                    return (Just pcrData)
                else return Nothing
            opcr <- if hasOriginalPcr
                then do
                    pcrData <- getPCR
                    return (Just pcrData)
                else return Nothing
            sc <- if isSplicingPoint
                then do
                    scd <- getWord8
                    return (Just (fromIntegral scd))
                else return Nothing
            pdata <- if hasPrivateData
                then do
                    pdlen <- getWord8
                    pdata <- getLazyByteString (fromIntegral pdlen)
                    return (Just pdata)
                else return Nothing
            afe <- if hasExtension
                then do
                    e <- get :: Get AdaptationFieldExtension
                    return (Just e)
                else return Nothing
            let remaining = fromIntegral aflength -
                    2 -
                    (if hasPcr then 6 else 0) -
                    (if hasOriginalPcr then 6 else 0) -
                    (if isSplicingPoint then 1 else 0) -
                    (maybe 0 (\pd -> fromIntegral (LB.length pd)) pdata) -
                    (maybe 0 (\e -> adaptationFieldExtensionLength e) afe)
            stuffing <- getByteString remaining
            return (AdaptationField { adaptationFieldFlags = flags,
                pcr = pcr,
                originalPcr = opcr,
                spliceCountdown = sc,
                privateData = pdata,
                extension = afe,
                stuffingBytes = remaining})

instance Show AdaptationFieldExtension where
    -- | Generates a human-readable representation of the MPEG-2 TS adaptation
    -- field extension.
    --
    -- The output format is similar to the description of the adaptation field
    -- extension in ISO/IEC 13818-1.
    show afe =
        "[Adaptation Field Extension start]\n" ++
        "length          : " ++ show (adaptationFieldExtensionLength afe) ++ "\n" ++
        "ltw             : " ++ maybe "none" show (ltw afe) ++ "\n" ++
        "piecewise_rate  : " ++ maybe "none" show (piecewiseRate afe) ++ "\n" ++
        "seamless_splice : " ++ maybe "none" show (seamlessSplice afe) ++ "\n" ++
        "extension data  : " ++ maybe "none" (\e -> show (LB.length e) ++ " bytes") (extensionData afe)

-- | 'AdaptationFieldExtension' is made an instance of 'Binary' so it can be
-- serialized and deserialized into 'ByteString' streams.
--
-- The serialization format follows the MPEG-2 TS adaptation field extension
-- specifications in ISO/IEC 13818-1, Table 2-6: Transport Stream adaptation
-- field, page 22.
instance Binary AdaptationFieldExtension where
    -- | Writes an adaptation field extension into the stream.
    put afe =
        let len = adaptationFieldExtensionLength afe
            flags = (maybe 0 (\_ -> bit 7) (ltw afe)) .|.
                (maybe 0 (\_ -> bit 6) (piecewiseRate afe)) .|.
                (maybe 0 (\_ -> bit 5) (seamlessSplice afe))
        in do
            when (len > 184) (fail "Adaptation field cannot be longer than 184 bytes")
            putWord8 (fromIntegral len)
            putWord8 flags
            maybe (return ()) (\ltw -> putWord16be (fromIntegral ltw)) (ltw afe)
            case piecewiseRate afe of
                Just pwr ->
                    let high = fromIntegral (pwr `shiftR` 8) :: Word16
                        low = fromIntegral pwr :: Word8
                    in do
                        putWord16be high
                        putWord8 low
                Nothing -> return ()
            case seamlessSplice afe of
                Just ss ->
                    let (high, low) = serializeSplice ss
                    in do
                        putWord32be high
                        putWord8 low
                Nothing -> return ()
            maybe (return ()) putLazyByteString (extensionData afe)
    -- | Reads an adaptation field extension from the stream.
    get = do
        len <- getWord8
        flags <- getWord8
        let hasLtw = testBit flags 7
            hasPiecewiseRate = testBit flags 6
            hasSeamlessSplice = testBit flags 5
        r_ltw <- if hasLtw
            then do
                rawltw <- getWord16be
                let iltw = fromIntegral rawltw :: Int
                return (Just iltw)
            else (return Nothing)
        r_piecewiseRate <- if hasPiecewiseRate
            then do
                high <- getWord16be
                low <- getWord8
                let ihigh = fromIntegral high :: Int
                    pwr = (ihigh `shiftL` 8) .|. fromIntegral low
                return (Just pwr)
            else return Nothing
        r_seamlessSplice <- if hasSeamlessSplice
            then do
                high <- getWord32be
                low <- getWord8
                let ss = deserializeSplice (high, low)
                return (Just ss)
            else return Nothing
        let remaining = (fromIntegral len) -
                2 -
                (if hasLtw then 2 else 0) -
                (if hasPiecewiseRate then 3 else 0) -
                (if hasSeamlessSplice then 5 else 0)
        r_data <- if remaining > 0
            then do
                dt <- getLazyByteString remaining
                return (Just dt)
            else return Nothing
        return AFE { ltw = r_ltw,
            piecewiseRate = r_piecewiseRate,
            seamlessSplice = r_seamlessSplice,
            extensionData = r_data }

pcrExtMask :: Word16
pcrExtMask = 0x1FF

spliceTypeMask :: Word32
spliceTypeMask = 0xF0000000

spliceSegment1Mask :: Word64
spliceSegment1Mask = 0xE00000000

spliceSegment2Mask :: Word64
spliceSegment2Mask = 0xFFFE0000

spliceSegment3Mask :: Word64
spliceSegment3Mask = 0xFFFE

serializeSplice :: SeamlessSplice -> (Word32, Word8)
serializeSplice (SeamlessSplice spliceType dtsNextAu) = (0, 0)

deserializeSplice :: (Word32, Word8) -> SeamlessSplice
deserializeSplice (high, low) = SeamlessSplice (fromIntegral spliceType) (fromIntegral splice)
    where
        spliceType = high `shiftR` 28
        spliceData = (fromIntegral high `shiftL` 16) .|. fromIntegral low :: Word64
        segment1 = spliceData .&. spliceSegment1Mask
        segment2 = spliceData .&. spliceSegment2Mask
        segment3 = spliceData .&. spliceSegment3Mask
        splice = (segment3 `shiftR` 1) .|. (segment2 `shiftR` 1) .|. (segment1 `shiftR` 1)

-- | Encodes a 'PCR' into serializable form.
serializePcr :: PCR -> (Word32, Word16)
serializePcr (PCR base ext) = (fromIntegral high, fromIntegral low)
    where
        high = base `shiftR` 1
        baseFinalBit = (base .&. 1) `shiftL` 15
        low = baseFinalBit .|. fromIntegral ext

-- | Decodes a 'PCR' from serializable form.
deserializePcr :: (Word32, Word16) -> PCR
deserializePcr (high, low) = PCR (fromIntegral base) (fromIntegral ext)
    where
        base = (high `shiftL` 1) .|. fromIntegral baseFinalBit
        baseFinalBit = low .&. bit 15
        ext = low .&. pcrExtMask
