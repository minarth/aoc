module Main where

import Data.List
-- CODE DEFINITION
-- ADDITION
op1 x y t xs = take t xs ++ [x + y] ++ drop (t + 1) xs
-- MULTIPLICATION
op2 x y t xs = take t xs ++ [x * y] ++ drop (t + 1) xs
-- INPUT
op3 t xs = take t xs ++ [input] ++ drop (t + 1) xs
op3In inVal t xs = take t xs ++ [(head inVal)] ++ drop (t + 1) xs

-- OUTPUT
op4 x xs out = (out ++ [xs !! x])

-- LESS THAN
op7 a b t xs
  | (a < b) = take t xs ++ [1] ++ drop (t + 1) xs
  | otherwise = take t xs ++ [0] ++ drop (t + 1) xs

-- EQUALS
op8 a b t xs
  | a == b = take t xs ++ [1] ++ drop (t + 1) xs
  | otherwise = take t xs ++ [0] ++ drop (t + 1) xs

output = []
input = [8]

--toDigits :: Integer -> [Integer]
toDigits n
  | n < 1 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- CMD is after applying toDigits
-- get value for operation, switch between position and immediate mode
value xs cmd pointer n
  | (length cmd) < (n+2) = (xs !! (xs !! (pointer + n)))
  | (cmd !! ((length cmd) - 2 - n)) == 1 = (xs !! (pointer + n))
  | otherwise = (xs !! (xs !! (pointer + n)))


getValue xs pointer n = (value xs (toDigits (xs !! pointer)) pointer n)
-- value [3, 8, 99, 8, 5, 98, 0, 0, 0, 0] (toDigits ([3, 8, 99, 8, 5, 98, 0, 0, 0, 0] !! 0)) 0 1

-- CMD is after applying toDigits
instruction cmd
  | and [(cmd !! ((length cmd) - 1)) == 9, (cmd !! ((length cmd) - 2)) == 9] = 99
  | otherwise = last cmd

getInstruction xs pointer = instruction (toDigits $ (xs !! pointer))

intcodeTest xs pointer out trace inVal
  | pointer >= length(xs) = [xs, out, (trace ++ [pointer, -1])]
  -- END INSTRUCTION
  | (getInstruction xs pointer) == 99 = [xs, out, (trace ++ [pointer, 99])]
  -- ADD INSTRUCTION
  | (getInstruction xs pointer) == 1 = intcodeTest
    (op1 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs)
    (pointer+4)
    out
    (trace ++ [pointer, 1])
    inVal
  -- MULTIPLY INSTRUCTION
  | (getInstruction xs pointer) == 2 = intcodeTest (op2 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer, 2]) inVal
  -- INPUT INSTRUCTION
  | and [((getInstruction xs pointer) == 3), (null inVal)] =  [xs, out, (trace ++ [pointer, 404])] -- TODO compute next
  | (getInstruction xs pointer) == 3 = intcodeTest (op3In inVal (xs !! (pointer + 1)) xs) (pointer+2) out (trace ++ [pointer, 3]) (tail inVal)
  -- OUTPUT INSTRUCTION
  | and [(getInstruction xs pointer) == 4, (xs !! pointer) > 100] = intcodeTest xs (pointer+2) (out ++ [(xs !! (pointer + 1))]) (trace ++ [pointer,14]) inVal
  | (getInstruction xs pointer) == 4 = (intcodeTest xs (pointer+2) (op4 (xs !! (pointer + 1)) xs out) (trace ++ [pointer,4])) inVal
  -- JUMP IF TRUE
  | and [(getInstruction xs pointer) == 5, (getValue xs pointer 1) /= 0] = intcodeTest xs (getValue xs pointer 2) out (trace ++ [pointer,15]) inVal
  | (getInstruction xs pointer) == 5 = intcodeTest xs (pointer+3) out (trace ++ [pointer,5]) inVal
  -- JUMP IF (FALSE ++ [pointer,])
  | and [(getInstruction xs pointer) == 6, (getValue xs pointer 1) == 0] = intcodeTest xs (getValue xs pointer 2) out (trace ++ [pointer,16]) inVal
  | (getInstruction xs pointer) == 6 = intcodeTest xs (pointer + 3) out (trace ++ [pointer,6]) inVal
  -- LESS (THAN ++ [pointer,])
  | (getInstruction xs pointer) == 7 = intcodeTest (op7 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer,7]) inVal
  -- (EQUALS ++ [pointer,])
  | (getInstruction xs pointer) == 8 = intcodeTest (op8 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer,8]) inVal
  | otherwise = [xs, out, (trace ++ [pointer, -2])]

prepProgram mem code n
  | n == 0 = mem
  |otherwise = prepProgram (mem ++ [[code, [], []]]) code (n-1)

-- FINISHED AFTER AMP E
run mem [] ampId
  | and [(ampId == 5),  (last (last (last mem)) == 99)] = mem
  | ampId == 5 = run (setMemory mem 0
    (intcodeTest
      (getMemory mem 0)
      (getLastPointer mem 0)
      [] []
      (getLastOutput mem 0)))
  [] 1
  | otherwise = run (setMemory mem ampId
    (intcodeTest
      (getMemory mem ampId)
      (getLastPointer mem ampId)
      [] []
      (getLastOutput mem ampId)))
  [] (ampId+1)
-- PREPARE THE INITIAL VALUES
run mem (x:xs) ampId
  | (length xs) == 0 = run (setMemory mem ampId (intcodeTest (getMemory mem ampId) 0 [] [] [x])) xs 0
  | otherwise = run (setMemory mem ampId (intcodeTest (getMemory mem ampId) 0 [] [] [x])) xs (ampId+1)

--inputData :: Int
--inputData = map fromIntegral [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,-1440,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,-3036,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,-85,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,-78,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,-4600,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,-6840,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,-154,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
--inputData = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,-1440,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,-3036,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,-85,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,-78,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,-4600,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,-6840,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,-154,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
inputData = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,(-1440),224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,(-3036),224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,(-85),224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,(-78),224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,(-4600),224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,(-125),224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,(-6840),224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,(-154),224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]

program = (map fromIntegral inputData)


getMemory :: [[[Int]]] -> Int -> [Int]
getMemory mem n = head (mem !! n)
getStack mem n = ((mem !! n) !! 2)

getLastPointer :: [[[Int]]] -> Int -> Int
getLastPointer mem n = ((getStack mem n) !! ((length (getStack mem n)) - 2))

getLastOutput :: [[[Int]]] -> Int -> [Int]
getLastOutput mem 0 = if (null (getOutput mem 4)) then [0] else (getOutput mem 4)
getLastOutput mem n = getOutput mem (n-1)

getOutput mem n = ((mem !! n) !! 1)
setMemory mem n val = take n mem ++ [val] ++ drop (n + 1) mem

-- program if iinput is equals 8
xs1 = [3,9,8,9,10,9,4,9,99,-1,8]
ys11 = intcodeTest xs1 0 [] [] [1]
ys13 = intcodeTest xs1 0 [] [] [3]
ys14 = intcodeTest xs1 0 [] [] [4]
ys18 = intcodeTest xs1 0 [] [] [8]
ys19 = intcodeTest xs1 0 [] [] [9]

-- program if input is less than 8
xs2 = [3,9,7,9,10,9,4,9,99,-1,8]
ys21 = intcodeTest xs2 0 [] [] [1]
ys23 = intcodeTest xs2 0 [] [] [3]
ys28 = intcodeTest xs2 0 [] [] [8]
ys29 = intcodeTest xs2 0 [] [] [9]


-- IMMEDIATE version of xs1
xs3 = [3,3,1108,-1,8,3,4,3,99]
ys31 = intcodeTest xs3 0 [] [] [1]
ys33 = intcodeTest xs3 0 [] [] [3]
ys38 = intcodeTest xs3 0 [] [] [8]
ys39 = intcodeTest xs3 0 [] [] [9]

-- IMMEDIATE version of xs2
xs4 = [3,3,1107,-1,8,3,4,3,99]
ys41 = intcodeTest xs4 0 [] [] [1]
ys43 = intcodeTest xs4 0 [] [] [3]
ys48 = intcodeTest xs4 0 [] [] [8]
ys49 = intcodeTest xs4 0 [] [] [9]

-- TESTING
inputTest :: [Int]
-- REAL TEST
inputTest = [3,8,1001,8,10,8,105,1,0,0,21,42,55,64,85,98,179,260,341,422,99999,3,9,101,2,9,9,102,5,9,9,1001,9,2,9,1002,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,3,9,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99]
-- IN 5
--inputTest = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
-- IN 4
--inputTest = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
-- IN 3
--inputTest = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
-- IN 2
--inputTest = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
-- IN 1
--inputTest = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
code :: [Int]
code = (map fromIntegral inputTest)
mem = prepProgram [] code 5

-- iterate through perms
perms :: [[Int]]
perms = permutations [5 .. 9]
findMax mem [] result = result
findMax mem (x:xs) result = findMax mem xs (result ++ (getOutput (run mem x 0) 4))

main = print(maximum (findMax mem perms []))