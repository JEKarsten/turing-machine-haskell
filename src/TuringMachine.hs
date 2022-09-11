module TuringMachine where

import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------
-- DATA TYPES
-----------------------------------------------

data Machine = Machine { all_states :: States,          -- stores all states in the Turing Machine
                         current_state :: StateID,      -- store the ID of the current state
                         machine_tape :: Tape }         -- represents the current symbols and position of the tape in the Turing Machine
               deriving (Eq)


type States = Map StateID State                         -- a map of all states that uses the StateID as the key and the State as the value


data State = State { state_transitions :: Transitions,  -- stores all transitions from that state
                     is_halt :: Halt }                  -- stores whether or not the state is a halting state
            deriving (Show, Eq)


type Transitions = Map Symbol Transition                -- a map of all transitions for a given state that uses the input Char as the key and a Transition as the value


type Transition = (Symbol, StateID, Direction)          -- a tuple representing a single transition: the new Char to write, new StateID to move to, and the Direction to move the tape


type Halt = Bool                                        -- represents whether or not the state is a halting state


data Direction = MovLeft | MovRight | Stay              -- instructs the tape which direction to move after wrting the new symbol
                 deriving (Eq)


type StateID = Int                                      -- stores the ID of the current state


data Tape = Tape { left_tape :: [Symbol],               -- a list of Symbols representing the left of the tape in reverse order (i.e. 0th symbol is directly to the left of the current pointer)
                   symbol :: Symbol,                    -- a Symbol representing the current symbol being pointed to on the tape
                   right_tape :: [Symbol] }             -- a list of Symbols representing the right of the tape
            deriving (Eq)


type Symbol = Char                                      -- represents the current symbol written on a spot of the tape




-----------------------------------------------
-- DISPLAY FUNCTIONS
-----------------------------------------------

-- displays the machine's current state (including where it's a halting state) and tape
instance Show Machine where
    show m@(Machine states id tape) =
        let
            state =
                if checkHalt m then "Halt   "
                else "State " ++ show id
        in
            "Config {" ++ "state = " ++ state ++ ", tape = " ++ show tape ++ "}"


-- displays the direction as a single character
instance Show Direction where
    show MovLeft = "L"
    show MovRight = "R"
    show Stay = "S"


-- displays a tape by reversing the left and then concatenating that with the current symbol (in brackets) and right
instance Show Tape where
    show (Tape left current right) = reverse left ++ '[' : current : ']' : right




-----------------------------------------------
-- TAPE FUNCTIONS
-----------------------------------------------

readTape :: Tape -> Symbol
-- | reads the current symbol on a tape
--   input: the tape to read (Tape)
--   output: the symbol at the tape head (Symbol)
readTape = symbol


writeTape :: Tape -> Symbol -> Tape
-- | writes a symbol to the current spot on the tape
--   input: the tape to write to (Tape)
--          the symbol to write (Symbol)
--   output: the updated tape (Tape)
writeTape (Tape left _ right) sym = Tape left sym right


moveTape :: Tape -> Direction -> Tape
-- | moves the tape
--   input: the tape to move (Tape)
--          the direction to move the tape (Direction)
--   output: the updated tape (Tape)
moveTape (Tape [] current right) MovLeft = Tape [] '_' (current:right)          -- moves tape left when left is empty
moveTape (Tape (l:left) current right) MovLeft = Tape left l (current:right)    -- moves tape left when left has at least 1 element

moveTape (Tape left current []) MovRight = Tape (current:left) '_' []           -- moves tape right when right is empty
moveTape (Tape left current (r:right)) MovRight = Tape (current:left) r right   -- moves tape right when right has at least 1 element

moveTape tape Stay = tape                                                       -- doesn't move tape


compareTapes :: Tape -> Tape -> String
-- | finds the difference between 2 tapes
--   input: the first tape (Tape)
--          the second tape (Tape)
--   output: a string with carrots (^) demarking different characters (String)
compareTapes tape1 tape2 =
    let
        tape1_str = plainTape tape1
        tape2_str = plainTape tape2
    in
        compareTapesHelper tape1_str tape2_str

compareTapesHelper :: String -> String -> String
-- | a helper function for compareTapes that performs recursion
--   input: a string representation of the first tape (String)
--          a string representation of the second tape (String)
--   output: a string with carrots (^) and spaces ( ) (String)
compareTapesHelper "" "" = ""
compareTapesHelper "" (c:rest) = '^' : compareTapesHelper "" rest
compareTapesHelper (c:rest) "" = '^' : compareTapesHelper rest ""
compareTapesHelper (c1:rest1) (c2:rest2) =
    if c1 == c2 then ' ' : compareTapesHelper rest1 rest2
    else '^' : compareTapesHelper rest1 rest2


plainTape :: Tape -> String
-- | returns a tape as a plain string of characters with no leading/trailing blanks
--   input: a tape (Tape)
--   output: a string with the characters on the tape (String)
plainTape tape =
    let tape_str = reverse (left_tape tape) ++ symbol tape : right_tape tape
    in reverse (trimTapeStart (reverse (trimTapeStart tape_str)))


trimTapeStart :: String -> String
-- | returns a string of a tape with no leading blanks
--   input: a string representing a tape (String)
--   output: the string with no leading blanks (String)
trimTapeStart ('_':rest) = trimTapeStart rest
trimTapeStart s = s


countChar :: Char -> String -> Int
-- | counts the number of instances of a character in a string
--   input: the character to count (Char)
--          the string to search (String)
--   output: the number of characters in the string (Int)
countChar _ "" = 0
countChar c (s:rest) =
    if c == s then 1 + countChar c rest
    else countChar c rest




-----------------------------------------------
-- STATE FUNCTIONS
-----------------------------------------------

getTransition :: Transitions -> Symbol -> Transition
-- | returns a tuple of the transition needed for the current symbol 
--   input: the transitions for a given state (Transitions)
--          the symbol being read (Symbol)
--   output: the transition to use (Transition)
getTransition transitions sym =  Map.findWithDefault ('0', 0, Stay) sym transitions




-----------------------------------------------
-- MACHINE FUNCTIONS
-----------------------------------------------

checkHalt :: Machine -> Bool
-- | checks if the machine is currently in a halting state
--   input: a machine (Machine)
--   output: True if the Machine is in a halting state, false otherwise (Bool)
checkHalt (Machine states id _) = is_halt (getState states id)


getState :: States -> StateID -> State
-- | returns the current State given a map of states and the current ID
--   input: all states for a machine (States)
--          the id of the desired state (StateID)
--   output: the desired state (State)
getState states id = Map.findWithDefault defaultState id states


step :: Machine -> Machine
-- | performs 1 step of computation on the input machine
--      (this includes reading the tape, acceessing the needed transition,
--       writing the tape, moving the tape, and updating the state_
--   input: a machine (Machine)
--   output: the machine after 1 step of computation (Machine)
step machine =
    let
        state = getState (all_states machine) (current_state machine)
        tape = machine_tape machine
        current_symbol = readTape tape
        (new_symbol, new_state_id, move_direction) = getTransition (state_transitions state) current_symbol
        new_tape = moveTape (writeTape tape new_symbol) move_direction
    in
        Machine (all_states machine) new_state_id new_tape


run :: Machine -> Machine
-- | runs a machine until reaching a halting state
--   input: a machine (Machine)
--   output: the machine after halting (Machine)
run start_m =
    if checkHalt start_m
        then start_m
    else run (step start_m)


simulate :: Machine -> String
-- | runs a machine until reaching a halting state and keeps a log of each step
--   input: a machine (Machine)
--   output: a string representing each step of computation (String)
simulate start_m =
    if checkHalt start_m
        then show start_m
    else show start_m ++ "\n" ++ simulate (step start_m)


simulateWithLog :: Machine -> (String, Int)
-- | runs a machine until reaching a halting state and keeps a log of each step
--   input: a machine (Machine)
--   output: a tuple containing...
--      a string representing each step of computation (String)
--      an integer tracking the number of steps (Int)
simulateWithLog start_m =
    if checkHalt start_m
        then (show start_m, 0)
    else
        let
            (config_line, num_iterations) = simulateWithLog (step start_m)
        in
            (show start_m ++ "\n" ++ config_line, num_iterations + 1)




-----------------------------------------------
-- MACHINE CONSTRUCTOR FUNCTIONS
-----------------------------------------------

makeMachine :: States -> Tape -> Machine
-- | creates a Turing machine given states and a tape
--   input: a map of states (States)
--          the starting tape (Tape)
--   output: the machine on State 0 (Machine)
makeMachine states = Machine states 0


defaultState :: State
-- | constructs a default state with no transitions (for Map.findWithDefault only)
--   output: an empty state (State)
defaultState = State Map.empty False


haltState :: State
-- | constructs a halting state with no transitions
--   output: an halting state (State)
haltState = State Map.empty True


makeState :: [(Symbol, Transition)] -> State
-- | constructs a state from a list of read-in symbols and their respective transitions
--   input: a list of tuples of read symbols and transition tuples ([(Symbol, Transition)])
--   output: a state (State)
makeState transitions_list = State (Map.fromList transitions_list) False


makeStates :: [(StateID, State)] -> States
-- | constructs a map of state from a list of IDs and their respective states
--   input: a list of tuples of state IDs and states ([(StateID, State)])
--   output: a map of states (States)
makeStates = Map.fromList


strToLeftTape :: String -> [Symbol]
-- | constructs a list of symbols that represents the left tape (which is in reverse)
--   input: a string representing the left tape as it appears (Symbol)
--   output: a list of symbols in reverse order ([Symbol])
strToLeftTape "" = []
strToLeftTape (c:rest) = strToLeftTape rest ++ [c]




-----------------------------------------------
-- TRACE FILE WRITERS
-----------------------------------------------

writeTraceFile :: Machine -> String -> IO ()
-- | writes a trace file of simulating a Turing machine
--   input: a machine (Machine)
--          the filename to write to (String)
--   output: an IO string in a .txt file (IO ())
writeTraceFile machine filename =
    writeFile ("../testing/" ++ filename ++ ".txt") (simulate machine)


writeTraceLogFile :: Machine -> String -> IO ()
-- | writes a trace and log file of simulating a Turing machine
--   input: a machine (Machine)
--          the filename to write to (String)
--   output: an IO string in a .txt file (IO ())
writeTraceLogFile machine filename =
    let
        start_tape = machine_tape machine
        Machine states _ end_tape = run machine
        (config_lines, num_iterations) = simulateWithLog machine
        tape_diff = compareTapes start_tape end_tape
        output =
            "Number of steps:  " ++ show num_iterations ++ "\n" ++
            "Starting tape:    " ++ plainTape start_tape ++ "\n" ++
            "Ending tape:      " ++ plainTape end_tape ++ "\n" ++
            "Tape differences: " ++ tape_diff ++ " " ++
            "(" ++ show (countChar '^' tape_diff) ++ " differences)" ++ "\n\n" ++
            config_lines
    in
        writeFile ("../testing/" ++ filename ++ ".txt") output