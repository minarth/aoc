-- PART ONE
-- ADDITION
op1 x y t xs = take t xs ++ [x + y] ++ drop (t + 1) xs 
-- MULTIPLICATION
op2 x y t xs = take t xs ++ [x * y] ++ drop (t + 1) xs
-- INPUT
op3 t xs = take t xs ++ [input] ++ drop (t + 1) xs 
op3In inVal t xs = take t xs ++ [inVal] ++ drop (t + 1) xs 


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
input = 8

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

intcode xs pointer out
  | pointer >= length(xs) = out
  -- EXIT
  | (getInstruction xs pointer) == 99 = out
  -- ADD
  | (getInstruction xs pointer) == 1 = intcode (op1 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out
  -- MULTIPLY
  | (getInstruction xs pointer) == 2 = intcode (op2 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out
  -- SET INPUT
  | (getInstruction xs pointer) == 3 = intcode (op3 (xs !! (pointer + 1)) xs) (pointer+2) out
  -- GET OUTPUT
  | and [(getInstruction xs pointer) == 4, (xs !! pointer) > 100] = intcode xs (pointer+2) (out ++ [(xs !! (pointer + 1))])
  -- GET OUTPUT
  | (getInstruction xs pointer) == 4 = intcode xs (pointer+2) (op4 (xs !! (pointer + 1)) xs out)
  -- TODO JUMP IF TRUE
  | and [(getInstruction xs pointer) == 5, (getValue xs pointer 1) /= 0] = intcode xs (getValue xs pointer 2) out
  | (getInstruction xs pointer) == 5 = intcode xs (pointer+3) out
  -- TODO JUMP IF FALSE
  | and [(getInstruction xs pointer) == 6, (getValue xs pointer 1) == 0] = intcode xs (getValue xs pointer 2) out
  | (getInstruction xs pointer) == 6 = intcode xs (pointer + 3) out
  -- LESS THAN
  | (getInstruction xs pointer) == 7 = intcode (op7 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out
  -- EQUALS
  | (getInstruction xs pointer) == 8 = intcode (op8 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out

  | otherwise =  out

intcodeTest xs pointer out trace inVal
  | pointer >= length(xs) = [xs, out, (trace ++ [pointer, -1])]
  | (getInstruction xs pointer) == 99 = [xs, out, (trace ++ [pointer, 99])]
--  | head xs > 19690720 = head xs; getValue xs pointer n
  | (getInstruction xs pointer) == 1 = intcodeTest (op1 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer, 1]) inVal
  | (getInstruction xs pointer) == 2 = intcodeTest (op2 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer, 2]) inVal
  | (getInstruction xs pointer) == 3 = intcodeTest (op3In inVal (xs !! (pointer + 1)) xs) (pointer+2) out (trace ++ [pointer, 3]) inVal
  | and [(getInstruction xs pointer) == 4, (xs !! pointer) > 100] = intcodeTest xs (pointer+2) (out ++ [(xs !! (pointer + 1))]) (trace ++ [pointer,14]) inVal
  | (getInstruction xs pointer) == 4 = (intcodeTest xs (pointer+2) (op4 (xs !! (pointer + 1)) xs out) (trace ++ [pointer,4])) inVal
  -- TODO JUMP IF TRUE
  | and [(getInstruction xs pointer) == 5, (getValue xs pointer 1) /= 0] = intcodeTest xs (getValue xs pointer 2) out (trace ++ [pointer,15]) inVal
  | (getInstruction xs pointer) == 5 = intcodeTest xs (pointer+3) out (trace ++ [pointer,5]) inVal
  -- TODO JUMP IF (FALSE ++ [pointer,])
  | and [(getInstruction xs pointer) == 6, (getValue xs pointer 1) == 0] = intcodeTest xs (getValue xs pointer 2) out (trace ++ [pointer,16]) inVal
  | (getInstruction xs pointer) == 6 = intcodeTest xs (pointer + 3) out (trace ++ [pointer,6]) inVal
  -- LESS (THAN ++ [pointer,])
  | (getInstruction xs pointer) == 7 = intcodeTest (op7 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer,7]) inVal
  -- (EQUALS ++ [pointer,])
  | (getInstruction xs pointer) == 8 = intcodeTest (op8 (getValue xs pointer 1) (getValue xs pointer 2) (xs !! (pointer + 3)) xs) (pointer+4) out (trace ++ [pointer,8]) inVal
  | otherwise = [xs, out, (trace ++ [pointer, -2])]

-- *Main> intcode [3,0,4,0,99] 0 []
-- [1]

-- *Main> intcode [1002,4,3,4,33] 0 []
-- []

-- * intcode [1101,100,-1,4,0] 0 []
-- []

--inputData :: Int
--inputData = map fromIntegral [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,-1440,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,-3036,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,-85,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,-78,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,-4600,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,-6840,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,-154,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
--inputData = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,-1440,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,-3036,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,-85,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,-78,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,-4600,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,-6840,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,-154,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
inputData = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,(-1440),224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,(-3036),224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,(-85),224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,(-78),224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,(-4600),224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,(-125),224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,(-6840),224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,(-154),224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]

xData = map fromIntegral inputData

-- program if iinput is equals 8
xs1 = [3,9,8,9,10,9,4,9,99,-1,8]
ys11 = intcodeTest xs1 0 [] [] 1
ys13 = intcodeTest xs1 0 [] [] 3
ys14 = intcodeTest xs1 0 [] [] 4
ys18 = intcodeTest xs1 0 [] [] 8
ys19 = intcodeTest xs1 0 [] [] 9

-- program if input is less than 8
xs2 = [3,9,7,9,10,9,4,9,99,-1,8]
ys21 = intcodeTest xs2 0 [] [] 1
ys23 = intcodeTest xs2 0 [] [] 3
ys28 = intcodeTest xs2 0 [] [] 8
ys29 = intcodeTest xs2 0 [] [] 9


-- IMMEDIATE version of xs1
xs3 = [3,3,1108,-1,8,3,4,3,99]
ys31 = intcodeTest xs3 0 [] [] 1
ys33 = intcodeTest xs3 0 [] [] 3
ys38 = intcodeTest xs3 0 [] [] 8
ys39 = intcodeTest xs3 0 [] [] 9

-- IMMEDIATE version of xs2
xs4 = [3,3,1107,-1,8,3,4,3,99]
ys41 = intcodeTest xs4 0 [] [] 1
ys43 = intcodeTest xs4 0 [] [] 3
ys48 = intcodeTest xs4 0 [] [] 8
ys49 = intcodeTest xs4 0 [] [] 9



---- xs = (map fromIntegral inputData) 

-- try_comb input 0 0
-- >> [22,54]

main = print("a")