module Test.AdaptationFieldTest where

import MPEG.TS.AdaptationField
import Data.Binary(encode, decode)
import Test.QuickCheck
import qualified Data.ByteString.Lazy as LB

testPcr = PCR 3321 1

minimalAf = AdaptationField [] Nothing Nothing Nothing Nothing Nothing 0

testAf = AdaptationField
    [Discontinuity, RandomAccess, Priority]
    (Just testPcr)
    (Just testPcr)
    (Just 32)
    (Just (LB.pack [0, 1, 2, 3, 4, 5, 6, 7]))
    Nothing
    7

testSplice = SeamlessSplice 0 5544321
testAfe = AFE (Just 8) (Just 332) (Just testSplice) (Just (LB.pack [0, 1, 2, 3, 4, 5, 6, 7]))

testAfWithExtension = AdaptationField
    [Discontinuity, RandomAccess, Priority]
    (Just testPcr)
    (Just testPcr)
    (Just 32)
    (Just (LB.pack [0, 1, 2, 3, 4 , 5, 6 ,7]))
    (Just testAfe)
    7

prop_spliceRoundTrip s = deserializeSplice (serializeSplice s) ==  s

prop_pcrRoundTrip pcr = deserializePcr (serializePcr pcr) == pcr

prop_minimalAdaptationFieldLength =
    adaptationFieldLength minimalAf == 1
    
prop_adaptationFieldLength =
    adaptationFieldLength testAf == 2 + 6 + 6 + 1 + 1 + 8 + 7
    
prop_adaptationFieldWithExtensionLength =
    adaptationFieldLength testAfWithExtension == 2 + 6 + 6 + 1 + 1 + 8 + 7 + 2 + 2 + 3 + 5 + 8

prop_afRoundTrip = decodedAf == testAfWithExtension
    where
        decodedAf = decode encodedAf
        encodedAf = encode testAfWithExtension
    