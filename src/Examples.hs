module Examples where
import TuringMachine
import CaesarCipher

-----------------------------------------------
-- TEST MACHINE STATES
-----------------------------------------------

machineStatesRemFirstZero :: States
-- | the states needed to remove the first zero from a block of characters
--   (used in example 1)
machineStatesRemFirstZero =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState [
            ('0', ('0', 4, MovLeft)),
            ('1', ('1', 4, MovRight)),
            ('_', ('_', 1, MovLeft))
            ]

        state1 = makeState [
            ('0', ('1', 2, MovRight)),
            ('1', ('1', 1, MovLeft)),
            ('_', ('1', 4, MovRight))
            ]
        
        state2 = makeState [
            ('0', ('0', 4, MovLeft)),
            ('1', ('1', 2, MovRight)),
            ('_', ('_', 3, MovLeft))
            ]

        state3 = makeState [
            ('0', ('0', 4, MovLeft)),
            ('1', ('_', 3, MovRight)),
            ('_', ('_', 4, MovLeft))
            ]
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, state2),
            (3, state3),
            (4, haltState)
            ]


machineStatesMoveHeadLeft :: States
-- | the states needed to move the tape head to the left of the block
--   (used in examples 2 and 3)
machineStatesMoveHeadLeft =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState [
            ('0', ('0', 2, MovLeft)),
            ('1', ('1', 2, MovRight)),
            ('_', ('_', 1, MovLeft))
            ]

        state1 = makeState [
            ('0', ('0', 1, MovLeft)),
            ('1', ('1', 1, MovLeft)),
            ('_', ('_', 2, MovRight))
            ]
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, haltState)
            ]


machineStatesFlip :: States
-- | the states needed to flip the bits of an input
--   (used in examples 4 and 5)
machineStatesFlip =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState [
            ('0', ('0', 3, MovLeft)),
            ('1', ('1', 3, MovRight)),
            ('_', ('_', 1, MovLeft))
            ]

        state1 = makeState [
            ('0', ('1', 1, MovLeft)),
            ('1', ('0', 1, MovLeft)),
            ('_', ('_', 2, MovRight))
            ]
        
        state2 = makeState [
            ('0', ('0', 2, MovRight)),
            ('1', ('1', 2, MovRight)),
            ('_', ('_', 3, MovRight))
            ]
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, state2),
            (3, haltState)
            ]


machineStatesRemZeros :: States
-- | the states needed to remove the block of zeros on the left of a block
--   (used in my example 1)
machineStatesRemZeros =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState [
            ('0', ('0', 2, MovLeft)),
            ('1', ('1', 2, MovRight)),
            ('_', ('_', 1, MovLeft))
            ]

        state1 = makeState [
            ('0', ('_', 1, MovLeft)),
            ('1', ('1', 2, MovRight)),
            ('_', ('_', 2, MovRight))
            ]
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, haltState)
            ]


machineStatesIncrement :: States
-- | the states needed to increment all digits (0-9) in a block of text
--   (used in my example 2)
machineStatesIncrement =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState [
            ('0', ('0', 2, MovRight)),
            ('1', ('1', 2, MovRight)),
            ('2', ('2', 2, MovRight)),
            ('3', ('3', 2, MovRight)),
            ('4', ('4', 2, MovRight)),
            ('5', ('5', 2, MovRight)),
            ('6', ('6', 2, MovRight)),
            ('7', ('7', 2, MovRight)),
            ('8', ('8', 2, MovRight)),
            ('9', ('9', 2, MovRight)),
            ('_', ('_', 1, MovLeft))
            ]

        state1 = makeState [
            ('0', ('1', 1, MovLeft)),
            ('1', ('2', 1, MovLeft)),
            ('2', ('3', 1, MovLeft)),
            ('3', ('4', 1, MovLeft)),
            ('4', ('5', 1, MovLeft)),
            ('5', ('6', 1, MovLeft)),
            ('6', ('7', 1, MovLeft)),
            ('7', ('8', 1, MovLeft)),
            ('8', ('9', 1, MovLeft)),
            ('9', ('0', 1, MovLeft)),
            ('_', ('_', 2, MovRight))
            ]
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, haltState)
            ]


machineStatesDecrement :: States
-- | the states needed to decrement all digits (0-9) in a block of text
--   (used in my example 3)
machineStatesDecrement =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState [
            ('0', ('0', 2, MovRight)),
            ('1', ('1', 2, MovRight)),
            ('2', ('2', 2, MovRight)),
            ('3', ('3', 2, MovRight)),
            ('4', ('4', 2, MovRight)),
            ('5', ('5', 2, MovRight)),
            ('6', ('6', 2, MovRight)),
            ('7', ('7', 2, MovRight)),
            ('8', ('8', 2, MovRight)),
            ('9', ('9', 2, MovRight)),
            ('_', ('_', 1, MovLeft))
            ]

        state1 = makeState [
            ('0', ('9', 1, MovLeft)),
            ('1', ('0', 1, MovLeft)),
            ('2', ('1', 1, MovLeft)),
            ('3', ('2', 1, MovLeft)),
            ('4', ('3', 1, MovLeft)),
            ('5', ('4', 1, MovLeft)),
            ('6', ('5', 1, MovLeft)),
            ('7', ('6', 1, MovLeft)),
            ('8', ('7', 1, MovLeft)),
            ('9', ('8', 1, MovLeft)),
            ('_', ('_', 2, MovRight))
            ]
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, haltState)
            ]


machineStatesCaesar :: Int -> Bool -> States
-- | the states needed to encode a caesar cipher machine
--   input: the key with which to encode the message (Int)
--          a boolean representing whether the machine should encipher (True) or decipher (False) (Bool)
--   output: the states necessary to encode the machine (States)
machineStatesCaesar key encipher =
    let
        -- (read symbol, (write symbol, new state, move direction))
        state0 = makeState (makeState0TransitionsCC 0)

        state1 = makeState (makeState1TransitionsCC 0 key encipher)
    in
        makeStates [
            (0, state0),
            (1, state1),
            (2, haltState)
            ]




-----------------------------------------------
-- TEST MACHINES WITH TAPES
-----------------------------------------------

machine1 :: Machine
-- | example machine 1; starting tape: 111011111[_]
machine1 = makeMachine
    machineStatesRemFirstZero
    (Tape (strToLeftTape "111011111") '_' [])

machine2 :: Machine
-- | example machine 2; starting tape: 11010[_]
machine2 = makeMachine
    machineStatesMoveHeadLeft
    (Tape (strToLeftTape "11010") '_' [])

machine3 :: Machine
-- | example machine 3; starting tape: 0101[_]
machine3 = makeMachine
    machineStatesMoveHeadLeft
    (Tape (strToLeftTape "0101") '_' [])

machine4 :: Machine
-- | example machine 4; starting tape: 11010[_]
machine4 = makeMachine
    machineStatesFlip
    (Tape (strToLeftTape "11010") '_' [])

machine5 :: Machine
-- | example machine 5; starting tape: 0101[_]
machine5 = makeMachine
    machineStatesFlip
    (Tape (strToLeftTape "0101") '_' [])

machine6 :: Machine
-- | example machine 6; starting tape: 010100[_]
machine6 = makeMachine
    machineStatesRemZeros
    (Tape (strToLeftTape "010100") '_' [])

machine7 :: Machine
-- | example machine 7; starting tape: 7349230[_]
machine7 = makeMachine
    machineStatesIncrement
    (Tape (strToLeftTape "7349230") '_' [])

machine8 :: Machine
-- | example machine 8; starting tape: 7349230[_]
machine8 = makeMachine
    machineStatesDecrement
    (Tape (strToLeftTape "7349230") '_' [])

machine9 :: Machine
-- | Caesar cipher; starting tape: hi[_]; key: 42; encipher
machine9 = makeMachine
    (machineStatesCaesar 42 True)
    (Tape (strToLeftTape "hi") '_' [])

machine10 :: Machine
-- | Caesar cipher; starting tape: NO[_]; key: 42; decipher
machine10 = makeMachine
    (machineStatesCaesar 42 False)
    (Tape (strToLeftTape "NO") '_' [])

machine11 :: Machine
-- | Caesar cipher; starting tape: Time to study for CS 105![_]; key: 470; encipher
machine11 = makeMachine
    (machineStatesCaesar 470 True)
    (Tape (strToLeftTape "Time to study for CS 105!") '_' [])

machine12 :: Machine
-- | Caesar cipher; starting tape: p480UD UCDEzIU1 BUYoULKPV[_]; key: 470; decipher
machine12 = makeMachine
    (machineStatesCaesar 470 False)
    (Tape (strToLeftTape "p480UD UCDEzIU1 BUYoULKPV") '_' [])




-----------------------------------------------
-- SIMULATE MACHINE FUNCTION
-----------------------------------------------

simulateHeader :: Int -> String
-- | Generates the header for a command line simulation
--   input: the number of the machine (Int)
--   output: the header (String)
simulateHeader i = "\nMachine " ++ show i ++ " Simulation:\n"


simulateFromString :: String -> String
-- | simulates a Turing machine based on the string name of the machine
--   input: the name of the machine (String)
--   output: the simulation of the machine (String)
simulateFromString "machine1"  = simulateHeader 1  ++ simulate machine1
simulateFromString "machine2"  = simulateHeader 2  ++ simulate machine2
simulateFromString "machine3"  = simulateHeader 3  ++ simulate machine3
simulateFromString "machine4"  = simulateHeader 4  ++ simulate machine4
simulateFromString "machine5"  = simulateHeader 5  ++ simulate machine5
simulateFromString "machine6"  = simulateHeader 6  ++ simulate machine6
simulateFromString "machine7"  = simulateHeader 7  ++ simulate machine7
simulateFromString "machine8"  = simulateHeader 8  ++ simulate machine8
simulateFromString "machine9"  = simulateHeader 9  ++ simulate machine9
simulateFromString "machine10" = simulateHeader 10 ++ simulate machine10
simulateFromString "machine11" = simulateHeader 11 ++ simulate machine11
simulateFromString "machine12" = simulateHeader 12 ++ simulate machine12
simulateFromString _ = "Couldn't find machine! Please try again :)"