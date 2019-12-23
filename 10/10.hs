import Data.Set (toList, fromList)

uniquify lst = toList $ fromList lst

-- :set -fbreak-on-exception

getAllAsteroids map (x,y) asteroids
  | y == (length map) = asteroids
  | x == (length (head map)) = getAllAsteroids map (0, y+1) asteroids
  | ((map !! y) !! x) == '#' = getAllAsteroids map (x+1, y) asteroids ++ [(x,y)]
  | otherwise = getAllAsteroids map (x+1, y) asteroids

angleToAsteroids _ [] angles = uniquify angles
angleToAsteroids (x1,y1) asteroids angles
  | and [(x1==(fst (head asteroids))), (y1==(snd (head asteroids)))] =
    angleToAsteroids (x1,y1) (tail asteroids) angles
  | otherwise = angleToAsteroids (x1,y1) (tail asteroids)
  (angles ++ [(atan2
    (fromIntegral (x1-(fst (head asteroids))))
    (fromIntegral (y1-(snd (head asteroids))))
  )])

anglesToAsteroids asteroids allAsteroids angles
  | null asteroids = angles
  | otherwise = anglesToAsteroids
    (tail asteroids)
    allAsteroids
    (angles ++ [(angleToAsteroids (head asteroids) allAsteroids [])])

------------------------
----------TEST----------
------------------------


x1 = [".#..#",".....","#####","....#","...##"]
astr1 = getAllAsteroids x1 (0,0) []
allAngles1 = map length (anglesToAsteroids astr1 astr1 [])

x2 = ["......#.#.","#..#.#....","..#######.",".#.#.###..",".#..#.....","..#....#.#","#..#....#.",".##.#..###","##...#..#.",".#....####"]
astr2 = getAllAsteroids x2 (0,0) []
allAngles2 = map length (anglesToAsteroids astr2 astr2 [])

