module TMToDot where

import TuringMachine
import Data.Map (Map)
import qualified Data.Map as Map


-----------------------------------------------
-- DOT HELPER FUNCTIONS
-----------------------------------------------

makeDotStates :: States -> StateID -> StateID -> String
-- | returns a dot string representing all transitions of all states in a machine
--   input: the states of a machine (States)
--          the ID of the current state, which is used for recursion (StateID)
--          the ID of the halting state, which is the last one (StateID)
--   output: a string in .dot syntax representing all states (String)
makeDotStates states 0 halt_state = ""
makeDotStates states n halt_state =
    let
        transitions = state_transitions (Map.findWithDefault defaultState (n-1) states)
        transition_list = Map.toList transitions
        transitions_repr =  makeDotTransitions transition_list n halt_state
    in
        makeDotStates states (n-1) halt_state ++ "\n" ++ transitions_repr


makeDotTransitions :: [(Symbol, Transition)] -> StateID -> StateID -> String
-- | returns a dot string representing all transitions for a given state
--   input: a list of tuples of the read symbol and its transition ([(Symbol, Transition)])
--          the ID of the current state (StateID)
--          the ID of the halting state (StateID)
--   output: a string in .dot syntax representing all transitions for a state (String)
makeDotTransitions [] _ _ = ""
makeDotTransitions ((read_sym, (write_sym, new_state, dir)):transition_list) state halt_state =
    makeDotTransition (state-1) read_sym write_sym new_state dir halt_state ++
    makeDotTransitions transition_list state halt_state


makeDotTransition :: StateID -> Symbol -> Symbol -> StateID -> Direction -> StateID -> String
-- | returns a dot string representing a given transition for a given state
--   input: the ID of the current state (StateID)
--          the read symbol (Symbol)
--          the write symbol (Symbol)
--          the ID state to transition to (StateID)
--          the direction to move the tape (Direction)
--          the ID of the halting state (StateID)
--   output: a string in .dot syntax representing the current transition (String)
makeDotTransition state read_sym write_sym new_state dir halt_state =
    let 
        new =
            if new_state == halt_state then "halt"
            else show new_state ++ "   "
    in
        show state ++ " -> " ++ new ++
        " [label = \" " ++ [read_sym] ++
        ":(" ++ [write_sym] ++ "," ++ show dir ++ ")\"];\n"


makeDotString :: States -> String
-- | returns a dot string representing all states for a given machine
--   input: a map of all states in a machine (States)
--   output: a string in .dot syntax representing the machine (String)
makeDotString states =
    let
        num_states = Map.size states
        states_dot_repr = makeDotStates states num_states (num_states-1)
    in
        "digraph D {\n\n" ++ "init -> 0;\n" ++
        states_dot_repr ++
        "halt -> halt;\n\n}"




-----------------------------------------------
-- DOT FILE WRITER
-----------------------------------------------

writeDotFile :: Machine -> String -> IO ()
-- | writes a dot file of a Turing machine
--   input: a machine (Machine)
--          the filename to write to (String)
--   output: an IO string in a .dot file (IO ())
writeDotFile machine filename =
    writeFile ("../testing/" ++ filename ++ ".dot") (makeDotString (all_states machine))