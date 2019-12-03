--let t = [[1,9,10,3,2,3,11,0,99,30,40,50], [1,0,0,0,99], [2,3,0,3,99], [2,4,4,5,99,0], [1,1,1,4,99,5,6,0,99]]
--test t
-- >>[3500,2,2,2,30,0]
--let input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,10,19,23,2,9,23,27,1,6,27,31,1,10,31,35,1,35,10,39,1,9,39,43,1,6,43,47,1,10,47,51,1,6,51,55,2,13,55,59,1,6,59,63,1,10,63,67,2,67,9,71,1,71,5,75,1,13,75,79,2,79,13,83,1,83,9,87,2,10,87,91,2,91,6,95,2,13,95,99,1,10,99,103,2,9,103,107,1,107,5,111,2,9,111,115,1,5,115,119,1,9,119,123,2,123,6,127,1,5,127,131,1,10,131,135,1,135,6,139,1,139,5,143,1,143,9,147,1,5,147,151,1,151,13,155,1,5,155,159,1,2,159,163,1,163,6,0,99,2,0,14,0]

-- PART ONE

op1 x y t xs = take t xs ++ [(xs !! x) + (xs !! y)] ++ drop (t + 1) xs
op2 x y t xs = take t xs ++ [(xs !! x) * (xs !! y)] ++ drop (t + 1) xs

intcode xs pointer
  | pointer >= length(xs) = head xs
  | xs !! pointer == 99 = head xs
  | head xs > 19690720 = head xs
  | xs !! pointer == 1 = intcode (op1 (xs !! (pointer + 1)) (xs !! (pointer + 2)) (xs !! (pointer + 3)) xs) (pointer+4)
  | xs !! pointer == 2 = intcode (op2 (xs !! (pointer + 1)) (xs !! (pointer + 2)) (xs !! (pointer + 3)) xs) (pointer+4)
  | otherwise =  head xs

test xss
  | length(xss) > 0 = [intcode (head xss) 0] ++ test(tail xss)
  | otherwise = [0]

-- PART TWO

prep xs noun verb = [(head xs), noun, verb] ++ drop 3 xs

try_comb xs noun verb
  | verb > 99 = try_comb xs (noun+1) 0
  | noun > 99 = [100, 100]
  | (intcode (prep xs noun verb) 0) == 19690720 = [noun, verb]
  | otherwise = try_comb xs noun (verb+1)

-- try_comb input 0 0
-- >> [22,54]

main = print("a")