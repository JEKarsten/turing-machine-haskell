module Testing where

import TuringMachine
import CaesarCipher
import TMToDot
import Examples
import Control.Monad (zipWithM_)

-----------------------------------------------
-- MACHINE LISTS
-----------------------------------------------

testingMachines :: [Machine]
testingMachines = [
    machine1,
    machine2,
    machine3,
    machine4,
    machine5,
    machine6,
    machine7,
    machine8,
    machine9,
    machine10,
    machine11,
    machine12
    ]


testingTraceFilenames :: [String]
testingTraceFilenames = [
    "machine01-trace",
    "machine02-trace",
    "machine03-trace",
    "machine04-trace",
    "machine05-trace",
    "machine06-trace",
    "machine07-trace",
    "machine08-trace",
    "machine09-trace",
    "machine10-trace",
    "machine11-trace",
    "machine12-trace"
    ]


testingTraceLogFilenames :: [String]
testingTraceLogFilenames = [
    "machine01-trace-log",
    "machine02-trace-log",
    "machine03-trace-log",
    "machine04-trace-log",
    "machine05-trace-log",
    "machine06-trace-log",
    "machine07-trace-log",
    "machine08-trace-log",
    "machine09-trace-log",
    "machine10-trace-log",
    "machine11-trace-log",
    "machine12-trace-log"
    ]


testingDotFilenames :: [String]
testingDotFilenames = [
    "machine01-dotfile",
    "machine02-dotfile",
    "machine03-dotfile",
    "machine04-dotfile",
    "machine05-dotfile",
    "machine06-dotfile",
    "machine07-dotfile",
    "machine08-dotfile",
    "machine09-dotfile",
    "machine10-dotfile",
    "machine11-dotfile",
    "machine12-dotfile"
    ]




-----------------------------------------------
-- IO FUNCTIONS
-----------------------------------------------

writeTraceFiles :: IO ()
writeTraceFiles = zipWithM_ writeTraceFile testingMachines testingTraceFilenames


writeTraceLogFiles :: IO ()
writeTraceLogFiles = zipWithM_ writeTraceLogFile testingMachines testingTraceLogFilenames


writeDotFiles :: IO ()
writeDotFiles = zipWithM_ writeDotFile testingMachines testingDotFilenames