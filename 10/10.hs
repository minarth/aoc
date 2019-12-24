import Data.Set (toList, fromList)
import Data.List (sortBy, sort, elemIndex)
import Data.Function (on)
import Data.Maybe (fromMaybe)


uniquify lst = toList $ fromList lst

-- :set -fbreak-on-exception

getAllAsteroids map (x,y) asteroids
  | y == (length map) = asteroids
  | x == (length (head map)) = getAllAsteroids map (0, y+1) asteroids
  | ((map !! y) !! x) == '#' = getAllAsteroids map (x+1, y) asteroids ++ [(x,y)]
  | otherwise = getAllAsteroids map (x+1, y) asteroids

angleToAsteroids _ [] angles = angles
angleToAsteroids (x1,y1) asteroids angles
  | and [(x1==(fst (head asteroids))), (y1==(snd (head asteroids)))] =
    angleToAsteroids (x1,y1) (tail asteroids) angles
  | otherwise = angleToAsteroids (x1,y1) (tail asteroids)
  (angles ++ [(atan2
    (fromIntegral (x1-(fst (head asteroids))))
    (fromIntegral (y1-(snd (head asteroids))))
  )])

distanceToAsteroids  _ [] dists = dists
distanceToAsteroids (x1,y1) asteroids dists
  | and [(x1==(fst (head asteroids))), (y1==(snd (head asteroids)))] =
    distanceToAsteroids (x1,y1) (tail asteroids) dists
  | otherwise = distanceToAsteroids (x1,y1) (tail asteroids)
  (dists ++ [sqrt ((fromIntegral (x1-(fst (head asteroids)))^2) +
    (fromIntegral (y1-(snd (head asteroids)))^2))
  ])

anglesToAsteroids asteroids allAsteroids angles
  | null asteroids = angles
  | otherwise = anglesToAsteroids
    (tail asteroids)
    allAsteroids
    (angles ++ [uniquify (angleToAsteroids (head asteroids) allAsteroids [])])

customZip asteroids datas zipped
  | null asteroids = zipped
  | otherwise = customZip (tail asteroids) (tail datas)
    (zipped ++ [[(fst (head datas)), (snd (head datas)), (fromIntegral (fst (head asteroids))), (fromIntegral (snd (head asteroids)))]])

customMerge targets target final
  | null targets = final ++ [((head (head target)), (sort target))]
  | null target = customMerge (tail targets) [(head targets)] final
  | (head (head targets)) == (head (head target)) = customMerge (tail targets) (target ++ [(head targets)]) final
  | otherwise = customMerge (tail targets) [(head targets)] (final ++ [((head (head target)), (sort target))])

takeAway [] _ = []
takeAway targets n = (take n targets) ++ (drop (n+1) targets)

shoot :: [[Double]] -> Double -> Int -> [[Double]] -> [[Double]]
shoot targets prevTarget n vaporized
  | (null targets) = vaporized
  | (length targets) == n = shoot targets prevTarget 0 vaporized
  | (head (targets !! n)) == prevTarget = shoot targets prevTarget (n+1) vaporized
  | otherwise = shoot (takeAway targets n) (head (targets !! n)) n (vaporized ++ [(targets !! n)])

takeAway2 [] _ = []
takeAway2 targets n
  | (length targets) <= n = targets
  | (length (snd (targets !! n))) == 1 = (take n targets) ++ (drop (n+1) targets)
  | otherwise = (take n targets) ++ [((fst (targets !! n)), (tail (snd (targets !! n))))] ++ (drop (n+1) targets)

shoot2 targets n prev vaporized
  | (null targets) = vaporized
  | (length targets) <= n = shoot2 targets 0 prev vaporized
  | ((fst (targets !! n)) == prev) = shoot2 targets (n+1) (-5.0) vaporized
  | otherwise = shoot2 (takeAway2 targets n) n (fst (targets !! n)) (vaporized ++ [(head (snd (targets !! n)))])
------------------------
----------TEST----------
------------------------


x1 = [".#..#",".....","#####","....#","...##"]
astr1 = getAllAsteroids x1 (0,0) []
allAngles1 = map length (anglesToAsteroids astr1 astr1 [])

x2 = ["......#.#.","#..#.#....","..#######.",".#.#.###..",".#..#.....","..#....#.#","#..#....#.",".##.#..###","##...#..#.",".#....####"]
astr2 = getAllAsteroids x2 (0,0) []
allAngles2 = map length (anglesToAsteroids astr2 astr2 [])

-- Big test for part 2

--xx = [".#....#####...#..", "##...##.#####..##", "##...#...#.#####.", "..#.....X...###..", "..#.#.....#....##"]
xx = [".#..##.###...#######", "##.############..##.", ".#.######.########.#", ".###.#######.####.#.", "#####.##.#.##.###.##", "..#####..#.#########", "####################", "#.####....###.#.#.##", "##.#################", "#####.##.###..####..", "..######..##.#######", "####.##.####...##..#", ".#####..#.######.###", "##...#.##########...", "#.##########.#######", ".####.#.###.###.#.##", "....##.##.###..#####", ".#.#.###########.###", "#.#.#.#####.####.###", "###.##.####.##.#..##"]
ast = getAllAsteroids xx (0,0) []
angles = angleToAsteroids (11, 13) ast []
dists = distanceToAsteroids (11, 13) ast []
datas = zip angles dists

targets = reverse $ sort $ customZip ast datas []
merged = customMerge targets [] []

shots = shoot2 merged 115 (-1.0) []

-- Test of input data
xxX = ["#.....#...#.........###.#........#..", "....#......###..#.#.###....#......##", "......#..###.......#.#.#.#..#.......", "......#......#.#....#.##....##.#.#.#", "...###.#.#.......#..#...............", "....##...#..#....##....#...#.#......", "..##...#.###.....##....#.#..##.##...", "..##....#.#......#.#...#.#...#.#....", ".#.##..##......##..#...#.....##...##", ".......##.....#.....##..#..#..#.....", "..#..#...#......#..##...#.#...#...##", "......##.##.#.#.###....#.#..#......#", "#..#.#...#.....#...#...####.#..#...#", "...##...##.#..#.....####.#....##....", ".#....###.#...#....#..#......#......", ".##.#.#...#....##......#.....##...##", ".....#....###...#.....#....#........", "...#...#....##..#.#......#.#.#......", ".#..###............#.#..#...####.##.", ".#.###..#.....#......#..###....##..#", "#......#.#.#.#.#.#...#.#.#....##....", ".#.....#.....#...##.#......#.#...#..", "...##..###.........##.........#.....", "..#.#..#.#...#.....#.....#...###.#..", ".#..........#.......#....#..........", "...##..#..#...#..#...#......####....", ".#..#...##.##..##..###......#.......", ".##.....#.......#..#...#..#.......#.", "#.#.#..#..##..#..............#....##", "..#....##......##.....#...#...##....", ".##..##..#.#..#.................####", "##.......#..#.#..##..#...#..........", "#..##...#.##.#.#.........#..#..#....", ".....#...#...#.#......#....#........", "....#......###.#..#......##.....#..#", "#..#...##.........#.....##.....#...."]
astX = getAllAsteroids xxX (0,0) []
baseIndex = fromMaybe (-1) (elemIndex (maximum $ map length (anglesToAsteroids astX astX [])) (map length (anglesToAsteroids astX astX [])))
basePos = astX !! baseIndex
astX2 = takeAway astX baseIndex

anglesX = angleToAsteroids basePos astX2 []
distsX = distanceToAsteroids basePos astX2 []
datasX = zip anglesX distsX

targetsX = reverse $ sort $ customZip astX2 datasX []
mergedX = customMerge targetsX [] []

startIndex = fromMaybe (-1) (elemIndex 0.0 [(fst x) | x <- mergedX])

shotsX = shoot2 mergedX startIndex (-1.0) []
