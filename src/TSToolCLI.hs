module Main where

import Data.Binary(encode)
import qualified Data.ByteString.Lazy as LB
import MPEG.Tools.Analyzer
import MPEG.Tools.PacketGenerator
import MPEG.Tools.StreamIO
import MPEG.Tools.TransportBuffer
import System.Environment(getArgs)
import System.IO
import System.IO.Error

data Command = Analyze | GenerateData | PacketOrder | PrintPackets deriving (Eq, Show)

getCommand :: String -> Command
getCommand "analyze" = Analyze
getCommand "gendata" = GenerateData
getCommand "packets" = PacketOrder
getCommand "dump" = PrintPackets
getCommand cmd = error ("Unknown command " ++ cmd)

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: tstool {command} [arguments]"
    putStrLn "The command can be:"
    putStrLn ""
    putStrLn "analyze\t\tPrints statistics about a transport stream."
    putStrLn "\t\tArguments: input file"
    putStrLn ""
    putStrLn "\t\t-o filename: Output to file. If this argument is not specified,\n\t\twrite to stdout"
    putStrLn "\t\tfilename: Input file. Specify '-' to read from stdin"
    putStrLn ""
    putStrLn "gendata\t\tGenerates dummy transport stream packets."
    putStrLn "\t\tArguments: [-o filename] [-n number of packets]"
    putStrLn ""
    putStrLn "\t\t-o filename: Output to file. If this argument is not specified,\n\t\twrite to stdout"
    putStrLn "\t\t-n packets : Number of transport stream packets to write"
    putStrLn ""
    putStrLn "packets\t\tDisplays the packet order of a transport stream."
    putStrLn "\t\tArguments: {filename}"
    putStrLn ""
    putStrLn "\t\tfilename: Input file. Specify '-' to read from stdin"
    putStrLn ""
    putStrLn "dump\t\tPrints the contents of transport stream packets."
    putStrLn "\t\tArguments: input file"

isHelpOption args = (elem "-h" args) || (elem "--help" args)

getNumber :: [String] -> Int
getNumber (arg : args)
    | arg == "-n" = read (head args)
    | otherwise = getNumber args

getInputStream :: [String] -> IO Handle
getInputStream args =
    let fileName = last args in
    if fileName == "-"
        then return stdin
        else do
            handle <- openBinaryFile fileName ReadMode
            return handle

getOutputStream :: [String] -> IO Handle
getOutputStream [] = return stdout
getOutputStream [_] = error "Not output file specified"
getOutputStream (arg : args)
    | arg == "-o" =
        let fileName = head args in do
                handle <- openBinaryFile fileName WriteMode
                return handle 
    | otherwise = getOutputStream args 

main = do
    args <- getArgs
    if length args == 0 || isHelpOption args
        then printUsage
        else
            let command = getCommand (args !! 0) in
            case command of
                Analyze -> do
                    file <- getInputStream args
                    processFile file analyzeStream
                    hClose file
                GenerateData ->
                    let n = getNumber args
                        packet = encode exampleTransportPacket
                    in do
                        output <- getOutputStream args
                        print exampleTransportPacket
                        LB.hPut output packet
                        hClose output
                PacketOrder -> do
                    file <- getInputStream args
                    printPacketOrder file
                    hClose file
                PrintPackets -> do
                    file <- getInputStream args
                    printPackets file
                    hClose file
    return 0
