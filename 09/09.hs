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
-- get value for operation, switch between position and immediate mode and relative mode
value xs cmd pointer n relativeBase
  -- Position mode
  | (length cmd) < (n+2) = (xs !! (xs !! (pointer + n)))
  -- Position mode
  | (cmd !! ((length cmd) - 2 - n)) == 0 = (xs !! (xs !! (pointer + n)))
  -- Immediate mode
  | (cmd !! ((length cmd) - 2 - n)) == 1 = (xs !! (pointer + n))
  -- Relative mode
  | (cmd !! ((length cmd) - 2 - n)) == 2 = (xs !! (relativeBase + (xs !! (pointer + n))))
  | otherwise = (xs !! (xs !! (pointer + n)))

getValue xs pointer n relativeBase = (value xs (toDigits (xs !! pointer)) pointer n relativeBase)
-- value [3, 8, 99, 8, 5, 98, 0, 0, 0, 0] (toDigits ([3, 8, 99, 8, 5, 98, 0, 0, 0, 0] !! 0)) 0 1

-- THIS WILL RETURN THE POINTER INDEX FOR THE COMMAND; NEEDED FOR MEM OVERFLOW
findPointer xs cmd pointer n relativeBase
  | (length cmd) < (n+2) = (xs !! (pointer + n))
  -- Position mode
  | (cmd !! ((length cmd) - 2 - n)) == 0 = (xs !! (pointer + n))
  -- Immediate mode
  | (cmd !! ((length cmd) - 2 - n)) == 1 = (pointer + n)
  -- Relative mode
  | (cmd !! ((length cmd) - 2 - n)) == 2 = (relativeBase + (xs !! (pointer + n)))
  | otherwise = (xs !! (pointer + n))

getPointer xs pointer n relativeBase = (findPointer xs (toDigits (xs !! pointer)) pointer n relativeBase)

-- CMD is after applying toDigits
instruction cmd
  | (cmd == [9,9]) = 99
  -- | and [(cmd !! ((length cmd) - 1)) == 9, (cmd !! ((length cmd) - 2)) == 9] = 99
  | otherwise = last cmd

getInstruction xs pointer = instruction (toDigits $ (xs !! pointer))

outOfScope xs pointer relativeBase
  -- OPS WITH 3 INPUTS
  | and[(elem (getInstruction xs pointer) [1,2,7,8]),
      ((maximum [
        (getPointer xs pointer 1 relativeBase),
        (getPointer xs pointer 2 relativeBase),
        (getPointer xs pointer 3 relativeBase)
        ]) >= (length xs))] = ((maximum [
        (getPointer xs pointer 1 relativeBase),
        (getPointer xs pointer 2 relativeBase),
        (getPointer xs pointer 3 relativeBase)
        ]) - (length xs) + 1)
  -- OPS WITH 2 INPUTS
  | and[(elem (getInstruction xs pointer) [5,6]),
      ((maximum [
        (getPointer xs pointer 1 relativeBase),
        (getPointer xs pointer 2 relativeBase)
        ]) >= (length xs))] = ((maximum [
        (getPointer xs pointer 1 relativeBase),
        (getPointer xs pointer 2 relativeBase)
        ]) - (length xs) + 1)
  -- OPS WITH 1 INPUT
  | and[(elem (getInstruction xs pointer) [3,4,7,8]),
      ((getPointer xs pointer 1 relativeBase)
         >= (length xs))] = ((getPointer xs pointer 1 relativeBase) - (length xs) + 1)
  -- ITS OK
  | otherwise = 0

intcodeTest xs pointer out trace inVal relativeBase
  | pointer >= length(xs) = [xs, out, (trace ++ [pointer, -1])]
  -- CHECK IF ACCESSING OUT OF MEMORY POINTER => MAKE BIGGER ARRAY
  | (outOfScope xs pointer relativeBase) > 0 = intcodeTest
      (xs ++ (replicate (outOfScope xs pointer relativeBase) 0))
      pointer
      out
      trace
      inVal
      relativeBase
  -- END
  | (getInstruction xs pointer) == 99 = [xs, out, (trace ++ [pointer, 99])]
  | (getInstruction xs pointer) == 1 = intcodeTest
      (op1
          (getValue xs pointer 1 relativeBase)
          (getValue xs pointer 2 relativeBase)
          (getPointer xs pointer 3 relativeBase) xs)
      (pointer+4)
      out
      (trace ++ [pointer, 1])
      inVal
      relativeBase
  | (getInstruction xs pointer) == 2 = intcodeTest
      (op2
        (getValue xs pointer 1 relativeBase)
        (getValue xs pointer 2 relativeBase)
        (getPointer xs pointer 3 relativeBase) xs)
      (pointer+4)
      out
      (trace ++ [pointer, 2])
      inVal
      relativeBase
  | (getInstruction xs pointer) == 3 = intcodeTest
      (op3In
        inVal
        (getPointer xs pointer 1 relativeBase) xs)
      (pointer+2)
      out
      (trace ++ [pointer, 3])
      inVal
      relativeBase
  | and [(getInstruction xs pointer) == 4, (xs !! pointer) > 200] = intcodeTest
      xs
      (pointer+2)
      (op4 (relativeBase + (xs !! (pointer + 1))) xs out)
      (trace ++ [pointer,24])
      inVal
      relativeBase
  | and [(getInstruction xs pointer) == 4, (xs !! pointer) > 100] = intcodeTest
      xs
      (pointer+2)
      (out ++ [(xs !! (pointer + 1))])
      (trace ++ [pointer,14])
      inVal
      relativeBase
  | (getInstruction xs pointer) == 4 = (intcodeTest
      xs
      (pointer+2)
      (op4 (xs !! (pointer + 1)) xs out)
      (trace ++ [pointer,4]))
      inVal
      relativeBase
  -- TODO JUMP IF TRUE
  | and [(getInstruction xs pointer) == 5, (getValue xs pointer 1 relativeBase) /= 0] = intcodeTest xs (getValue xs pointer 2 relativeBase) out (trace ++ [pointer,15]) inVal relativeBase
  | (getInstruction xs pointer) == 5 = intcodeTest xs (pointer+3) out (trace ++ [pointer,5]) inVal relativeBase
  -- TODO JUMP IF (FALSE ++ [pointer,])
  | and [(getInstruction xs pointer) == 6, (getValue xs pointer 1 relativeBase) == 0] = intcodeTest xs (getValue xs pointer 2 relativeBase) out (trace ++ [pointer,16]) inVal relativeBase
  | (getInstruction xs pointer) == 6 = intcodeTest xs (pointer + 3) out (trace ++ [pointer,6]) inVal relativeBase
  -- LESS (THAN ++ [pointer,])
  | (getInstruction xs pointer) == 7 = intcodeTest (op7 (getValue xs pointer 1 relativeBase) (getValue xs pointer 2 relativeBase) (getPointer xs pointer 3 relativeBase) xs) (pointer+4) out (trace ++ [pointer,7]) inVal relativeBase
  -- (EQUALS ++ [pointer,])
  | (getInstruction xs pointer) == 8 = intcodeTest (op8 (getValue xs pointer 1 relativeBase) (getValue xs pointer 2 relativeBase) (getPointer xs pointer 3 relativeBase) xs) (pointer+4) out (trace ++ [pointer,8]) inVal relativeBase
  -- UPDATE RELATIVE BASE
  | (getInstruction xs pointer) == 9 = intcodeTest xs (pointer+2) out (trace ++ [pointer, 9]) inVal (relativeBase + (getValue xs pointer 1 relativeBase))

  | otherwise = [xs, out, (trace ++ [pointer, -2])]


-- Simplification of a run
intcode xs inVal = ((intcodeTest xs 0 [] [] inVal 0) !! 1)


-----------------
---- TESTING ----
-----------------


--inputData :: Int
--inputData = map fromIntegral [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,-1440,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,-3036,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,-85,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,-78,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,-4600,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,-6840,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,-154,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
--inputData = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,72,20,224,1001,224,-1440,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1002,147,33,224,101,-3036,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,32,90,225,101,65,87,224,101,-85,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,33,92,225,1102,20,52,225,1101,76,89,225,1,117,122,224,101,-78,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,54,22,225,1102,5,24,225,102,50,84,224,101,-4600,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,92,64,225,1101,42,83,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,58,195,224,1001,224,-6840,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1101,76,48,225,1001,92,65,224,1001,224,-154,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,226,224,1002,223,2,223,1005,224,329,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,659,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
inputData = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,252,1,1023,1102,36,1,1008,1102,24,1,1017,1101,25,0,1013,1102,479,1,1026,1101,0,259,1022,1102,1,38,1001,1102,1,713,1024,1101,0,708,1025,1102,1,22,1006,1101,0,32,1010,1101,476,0,1027,1102,1,516,1029,1102,1,34,1009,1101,0,23,1016,1102,1,37,1011,1102,525,1,1028,1101,0,35,1004,1102,31,1,1002,1102,39,1,1019,1102,28,1,1015,1102,1,1,1021,1101,0,30,1007,1101,0,27,1014,1101,21,0,1018,1101,0,29,1005,1102,26,1,1000,1102,1,0,1020,1101,0,20,1012,1101,33,0,1003,109,13,21108,40,40,6,1005,1019,199,4,187,1106,0,203,1001,64,1,64,1002,64,2,64,109,15,1205,-7,221,4,209,1001,64,1,64,1105,1,221,1002,64,2,64,109,-25,1208,-3,26,63,1005,63,243,4,227,1001,64,1,64,1106,0,243,1002,64,2,64,109,25,2105,1,-5,1001,64,1,64,1106,0,261,4,249,1002,64,2,64,109,-4,21108,41,42,-8,1005,1016,281,1001,64,1,64,1106,0,283,4,267,1002,64,2,64,109,-6,1206,2,301,4,289,1001,64,1,64,1105,1,301,1002,64,2,64,109,-4,21102,42,1,2,1008,1016,42,63,1005,63,323,4,307,1106,0,327,1001,64,1,64,1002,64,2,64,109,-7,2108,35,1,63,1005,63,343,1105,1,349,4,333,1001,64,1,64,1002,64,2,64,109,-13,1208,7,35,63,1005,63,369,1001,64,1,64,1106,0,371,4,355,1002,64,2,64,109,24,21102,43,1,-1,1008,1017,42,63,1005,63,391,1105,1,397,4,377,1001,64,1,64,1002,64,2,64,109,-13,2101,0,-4,63,1008,63,38,63,1005,63,419,4,403,1105,1,423,1001,64,1,64,1002,64,2,64,109,21,1206,-5,435,1106,0,441,4,429,1001,64,1,64,1002,64,2,64,109,-22,21101,44,0,10,1008,1014,44,63,1005,63,463,4,447,1105,1,467,1001,64,1,64,1002,64,2,64,109,25,2106,0,-2,1106,0,485,4,473,1001,64,1,64,1002,64,2,64,109,-19,2107,37,-2,63,1005,63,501,1106,0,507,4,491,1001,64,1,64,1002,64,2,64,109,8,2106,0,10,4,513,1001,64,1,64,1105,1,525,1002,64,2,64,109,-6,21107,45,46,0,1005,1012,547,4,531,1001,64,1,64,1105,1,547,1002,64,2,64,109,-5,1202,-1,1,63,1008,63,21,63,1005,63,567,1105,1,573,4,553,1001,64,1,64,1002,64,2,64,109,2,1207,-3,21,63,1005,63,589,1105,1,595,4,579,1001,64,1,64,1002,64,2,64,109,1,1201,-8,0,63,1008,63,34,63,1005,63,619,1001,64,1,64,1106,0,621,4,601,1002,64,2,64,109,-6,2102,1,-1,63,1008,63,33,63,1005,63,643,4,627,1105,1,647,1001,64,1,64,1002,64,2,64,109,10,21101,46,0,3,1008,1017,43,63,1005,63,667,1106,0,673,4,653,1001,64,1,64,1002,64,2,64,109,-13,2102,1,8,63,1008,63,35,63,1005,63,697,1001,64,1,64,1106,0,699,4,679,1002,64,2,64,109,23,2105,1,0,4,705,1105,1,717,1001,64,1,64,1002,64,2,64,109,-1,1205,-3,729,1106,0,735,4,723,1001,64,1,64,1002,64,2,64,109,-15,2101,0,0,63,1008,63,38,63,1005,63,755,1106,0,761,4,741,1001,64,1,64,1002,64,2,64,109,-2,2107,28,-1,63,1005,63,779,4,767,1106,0,783,1001,64,1,64,1002,64,2,64,109,-2,2108,35,0,63,1005,63,801,4,789,1105,1,805,1001,64,1,64,1002,64,2,64,109,1,1201,-5,0,63,1008,63,26,63,1005,63,831,4,811,1001,64,1,64,1105,1,831,1002,64,2,64,109,-5,1207,5,30,63,1005,63,849,4,837,1106,0,853,1001,64,1,64,1002,64,2,64,109,2,1202,-2,1,63,1008,63,26,63,1005,63,879,4,859,1001,64,1,64,1105,1,879,1002,64,2,64,109,15,21107,47,46,0,1005,1017,899,1001,64,1,64,1105,1,901,4,885,4,64,99,21102,1,27,1,21101,915,0,0,1106,0,922,21201,1,66416,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,22102,1,-2,-2,109,-3,2105,1,0]

xData = map fromIntegral inputData

-- program if iinput is equals 8
xs1 = [3,9,8,9,10,9,4,9,99,-1,8]
ys11 = intcodeTest xs1 0 [] [] 1 0
ys13 = intcodeTest xs1 0 [] [] 3 0
ys14 = intcodeTest xs1 0 [] [] 4 0
ys18 = intcodeTest xs1 0 [] [] 8 0
ys19 = intcodeTest xs1 0 [] [] 9 0

-- program if input is less than 8
xs2 = [3,9,7,9,10,9,4,9,99,-1,8]
ys21 = intcodeTest xs2 0 [] [] 1 0
ys23 = intcodeTest xs2 0 [] [] 3 0
ys28 = intcodeTest xs2 0 [] [] 8 0
ys29 = intcodeTest xs2 0 [] [] 9 0


-- IMMEDIATE version of xs1
xs3 = [3,3,1108,-1,8,3,4,3,99]
ys31 = intcodeTest xs3 0 [] [] 1 0
ys33 = intcodeTest xs3 0 [] [] 3 0
ys38 = intcodeTest xs3 0 [] [] 8 0
ys39 = intcodeTest xs3 0 [] [] 9 0

-- IMMEDIATE version of xs2
xs4 = [3,3,1107,-1,8,3,4,3,99]
ys41 = intcodeTest xs4 0 [] [] 1 0
ys43 = intcodeTest xs4 0 [] [] 3 0
ys48 = intcodeTest xs4 0 [] [] 8 0
ys49 = intcodeTest xs4 0 [] [] 9 0


xxx = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,252,1,1023,1102,36,1,1008,1102,24,1,1017,1101,25,0,1013,1102,479,1,1026,1101,0,259,1022,1102,1,38,1001,1102,1,713,1024,1101,0,708,1025,1102,1,22,1006,1101,0,32,1010,1101,476,0,1027,1102,1,516,1029,1102,1,34,1009,1101,0,23,1016,1102,1,37,1011,1102,525,1,1028,1101,0,35,1004,1102,31,1,1002,1102,39,1,1019,1102,28,1,1015,1102,1,1,1021,1101,0,30,1007,1101,0,27,1014,1101,21,0,1018,1101,0,29,1005,1102,26,1,1000,1102,1,0,1020,1101,0,20,1012,1101,33,0,1003,109,13,21108,40,40,6,1005,1019,199,4,187,1106,0,203,1001,64,1,64,1002,64,2,64,109,15,1205,-7,221,4,209,1001,64,1,64,1105,1,221,1002,64,2,64,109,-25,1208,-3,26,63,1005,63,243,4,227,1001,64,1,64,1106,0,243,1002,64,2,64,109,25,2105,1,-5,1001,64,1,64,1106,0,261,4,249,1002,64,2,64,109,-4,21108,41,42,-8,1005,1016,281,1001,64,1,64,1106,0,283,4,267,1002,64,2,64,109,-6,1206,2,301,4,289,1001,64,1,64,1105,1,301,1002,64,2,64,109,-4,21102,42,1,2,1008,1016,42,63,1005,63,323,4,307,1106,0,327,1001,64,1,64,1002,64,2,64,109,-7,2108,35,1,63,1005,63,343,1105,1,349,4,333,1001,64,1,64,1002,64,2,64,109,-13,1208,7,35,63,1005,63,369,1001,64,1,64,1106,0,371,4,355,1002,64,2,64,109,24,21102,43,1,-1,1008,1017,42,63,1005,63,391,1105,1,397,4,377,1001,64,1,64,1002,64,2,64,109,-13,2101,0,-4,63,1008,63,38,63,1005,63,419,4,403,1105,1,423,1001,64,1,64,1002,64,2,64,109,21,1206,-5,435,1106,0,441,4,429,1001,64,1,64,1002,64,2,64,109,-22,21101,44,0,10,1008,1014,44,63,1005,63,463,4,447,1105,1,467,1001,64,1,64,1002,64,2,64,109,25,2106,0,-2,1106,0,485,4,473,1001,64,1,64,1002,64,2,64,109,-19,2107,37,-2,63,1005,63,501,1106,0,507,4,491,1001,64,1,64,1002,64,2,64,109,8,2106,0,10,4,513,1001,64,1,64,1105,1,525,1002,64,2,64,109,-6,21107,45,46,0,1005,1012,547,4,531,1001,64,1,64,1105,1,547,1002,64,2,64,109,-5,1202,-1,1,63,1008,63,21,63,1005,63,567,1105,1,573,4,553,1001,64,1,64,1002,64,2,64,109,2,1207,-3,21,63,1005,63,589,1105,1,595,4,579,1001,64,1,64,1002,64,2,64,109,1,1201,-8,0,63,1008,63,34,63,1005,63,619,1001,64,1,64,1106,0,621,4,601,1002,64,2,64,109,-6,2102,1,-1,63,1008,63,33,63,1005,63,643,4,627,1105,1,647,1001,64,1,64,1002,64,2,64,109,10,21101,46,0,3,1008,1017,43,63,1005,63,667,1106,0,673,4,653,1001,64,1,64,1002,64,2,64,109,-13,2102,1,8,63,1008,63,35,63,1005,63,697,1001,64,1,64,1106,0,699,4,679,1002,64,2,64,109,23,2105,1,0,4,705,1105,1,717,1001,64,1,64,1002,64,2,64,109,-1,1205,-3,729,1106,0,735,4,723,1001,64,1,64,1002,64,2,64,109,-15,2101,0,0,63,1008,63,38,63,1005,63,755,1106,0,761,4,741,1001,64,1,64,1002,64,2,64,109,-2,2107,28,-1,63,1005,63,779,4,767,1106,0,783,1001,64,1,64,1002,64,2,64,109,-2,2108,35,0,63,1005,63,801,4,789,1105,1,805,1001,64,1,64,1002,64,2,64,109,1,1201,-5,0,63,1008,63,26,63,1005,63,831,4,811,1001,64,1,64,1105,1,831,1002,64,2,64,109,-5,1207,5,30,63,1005,63,849,4,837,1106,0,853,1001,64,1,64,1002,64,2,64,109,2,1202,-2,1,63,1008,63,26,63,1005,63,879,4,859,1001,64,1,64,1105,1,879,1002,64,2,64,109,15,21107,47,46,0,1005,1017,899,1001,64,1,64,1105,1,901,4,885,4,64,99,21102,1,27,1,21101,915,0,0,1106,0,922,21201,1,66416,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,942,1,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,22102,1,-2,-2,109,-3,2105,1,0]
-- DAY 1
intcode xxx 1

-- DAY 2
intcode xxx 2

---- xs = (map fromIntegral inputData)

-- try_comb input 0 0
-- >> [22,54]

main = print("a")