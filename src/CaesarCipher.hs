module CaesarCipher where

import TuringMachine
import Data.Char

-----------------------------------------------
-- FUNCTIONS
-----------------------------------------------

getCharFromInt :: Int -> Char
-- | returns the ASCII character of the input value; 63 total (numbers & letters only)
--   input: an integer that enumerates a character (Int)
--   output: the respective ASCII character (Char)
getCharFromInt 0  = 'A'
getCharFromInt 1  = 'B'
getCharFromInt 2  = 'C'
getCharFromInt 3  = 'D'
getCharFromInt 4  = 'E'
getCharFromInt 5  = 'F'
getCharFromInt 6  = 'G'
getCharFromInt 7  = 'H'
getCharFromInt 8  = 'I'
getCharFromInt 9  = 'J'
getCharFromInt 10 = 'K'
getCharFromInt 11 = 'L'
getCharFromInt 12 = 'M'
getCharFromInt 13 = 'N'
getCharFromInt 14 = 'O'
getCharFromInt 15 = 'P'
getCharFromInt 16 = 'Q'
getCharFromInt 17 = 'R'
getCharFromInt 18 = 'S'
getCharFromInt 19 = 'T'
getCharFromInt 20 = 'U'
getCharFromInt 21 = 'V'
getCharFromInt 22 = 'W'
getCharFromInt 23 = 'X'
getCharFromInt 24 = 'Y'
getCharFromInt 25 = 'Z'
getCharFromInt 26 = 'a'
getCharFromInt 27 = 'b'
getCharFromInt 28 = 'c'
getCharFromInt 29 = 'd'
getCharFromInt 30 = 'e'
getCharFromInt 31 = 'f'
getCharFromInt 32 = 'g'
getCharFromInt 33 = 'h'
getCharFromInt 34 = 'i'
getCharFromInt 35 = 'j'
getCharFromInt 36 = 'k'
getCharFromInt 37 = 'l'
getCharFromInt 38 = 'm'
getCharFromInt 39 = 'n'
getCharFromInt 40 = 'o'
getCharFromInt 41 = 'p'
getCharFromInt 42 = 'q'
getCharFromInt 43 = 'r'
getCharFromInt 44 = 's'
getCharFromInt 45 = 't'
getCharFromInt 46 = 'u'
getCharFromInt 47 = 'v'
getCharFromInt 48 = 'w'
getCharFromInt 49 = 'x'
getCharFromInt 50 = 'y'
getCharFromInt 51 = 'z'
getCharFromInt 52 = '0'
getCharFromInt 53 = '1'
getCharFromInt 54 = '2'
getCharFromInt 55 = '3'
getCharFromInt 56 = '4'
getCharFromInt 57 = '5'
getCharFromInt 58 = '6'
getCharFromInt 59 = '7'
getCharFromInt 60 = '8'
getCharFromInt 61 = '9'
getCharFromInt 62 = ' '
getCharFromInt 63 = '!'
getCharFromInt n = getCharFromInt (mod n 64)


makeState0TransitionsCC :: Int -> [(Symbol, Transition)]
-- | constructs the transitions for State 0 of a Caeaar cipher machine
--   input: an integer that goes through characters in the alphabet; input should start at 0 (Int)
--   output: a list of tuples of the read symbols and their transitions ([(Symbol, Transition)])
makeState0TransitionsCC 64 = [('_', ('_', 1, MovLeft))]
makeState0TransitionsCC n =
    let c = getCharFromInt n
    in (c, (c, 2, MovRight)) : makeState0TransitionsCC (n+1)


makeState1TransitionsCC :: Int -> Int -> Bool -> [(Symbol, Transition)]
-- | constructs the transitions for State 1 of a Caeaar cipher machine
--   input: an integer that goes through characters in the alphabet; input should start at 0 (Int)
--   output: a list of tuples of the read symbols and their transitions ([(Symbol, Transition)])
makeState1TransitionsCC 64 _ _ = [('_', ('_', 2, MovRight))]
makeState1TransitionsCC n key encipher =
    let
        read_symbol = getCharFromInt n
        write_symbol = 
            if encipher then getCharFromInt (n+key)
            else getCharFromInt (n-key)
    in
        (read_symbol, (write_symbol, 1, MovLeft)) : makeState1TransitionsCC (n+1) key encipher




-----------------------------------------------
-- OLD FUNCTIONS
-----------------------------------------------

-- NOTE: These don't currently work since they mess up how the .dot file compiles

getCharFromIntOld :: Int -> Char
-- | returns the ASCII character of the input value
--   (starts at '!' (33) and ends at ']' (93)); 61 total
--   input: an integer that enumerates a character (Int)
--   output: the respective ASCII character (Char)
getCharFromIntOld i =
    let n = 33 + mod i 61
    in toEnum n::Char


getIntFromCharOld :: Char -> Int
-- | returns the value of the input ASCII character
--   input: an ASCII character (Char)
--   output: an integer that enumerates the character (Int)
getIntFromCharOld = ord