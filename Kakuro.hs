import Text.Printf
import Data.List

data Cell = Empty
            | Across Int
            | Down Int
            | DownAcross Int Int
            | Value [Int]

e = Empty
a = Across
d = Down
da = DownAcross
v = Value [1 .. 9]
vv values = Value values

draw (Empty) = "   -----  "
draw (Down n) =  printf "   %2d\\--  " n
draw (Across n) = printf "   --\\%2d  " n
draw (DownAcross down across) = (printf "   %2d" down) ++ printf "\\%2d  " across
draw (Value values) = 
      if 1 == length values then 
        concatMap (\ x -> "     " ++ show x ++ "    ") values
      else 
         " " ++ concatMap (\ x -> if x `elem` values then show x else ".") [ 1..9 ]


drawRow row = (concatMap draw row) ++ "\n"

drawGrid grid = "\n" ++ concatMap drawRow grid

allDifferent nums = (length nums) == length (nub nums)


permute vs target soFar = 
    if target >= 1 then 
        if length soFar == (length vs) - 1 then [ soFar ++ [ target ] ]
        else 
            case vs !! (length soFar) of
              Value values -> concatMap (\ v -> permute vs (target - v) (soFar ++ [ v ])) values
              _ -> []
    else []

permuteAll vs total = permute vs total []

isPossible cell n =
  case cell of
    Value values -> n `elem` values
    _ -> False

solveStep cells total = 
    let final = (length cells) - 1 in
    map (\ p -> Value(p))
        (transpose
          (filter allDifferent
            (filter (\ p -> isPossible (cells !! final) (p !! final)) (permuteAll cells total))))

rowTarget (Across n) = n
rowTarget (DownAcross _ a) = a
rowTarget _ = 0

colTarget (Down d) = d
colTarget (DownAcross d _) = d
colTarget _ = 0

solvePair f (nvs : vs : _) = nvs ++ (solveStep vs (f (last nvs))) 
solvePair f (nvs : _) = nvs
solvePair f pair = []

solvePairRow = solvePair rowTarget

solvePairCol = solvePair colTarget

partitionBy f [] = []
partitionBy f coll@(x : xs) = 
        let fx = f x
            run = takeWhile (\ y -> fx == f y) coll
        in
        [ run ] ++ (partitionBy f (drop (length run) coll))

partitionAll n step [] = []
partitionAll n step coll = [(take n coll)] ++ (partitionAll n step (drop step coll))

partitionN n coll = partitionAll n n coll

isValue (Value _) = True
isValue _ = False

solveLine f cells = concatMap f (partitionN 2 (partitionBy isValue cells))

solveRow = solveLine solvePairRow

solveCol = solveLine solvePairCol

solveGrid grid = 
  transpose (map solveCol (transpose (map solveRow grid)))

solver :: [[Cell]] -> [[Cell]]
solver grid =
  let g = solveGrid grid
  in
  if g == grid then
    grid
  else
    solver g

grid1 = [ [ e, (d 4), (d 22), e, (d 16), (d 3) ],
          [ (a 3), v, v, (da 16 6), v, v ],
          [ (a 18), v, v, v, v, v ],
          [ e, (da 17 23), v, v, v, (d 14) ],
          [ (a 9), v, v, (a 6), v, v ],
          [ (a 15), v, v, (a 12), v, v ] ]

main = do
  putStrLn (draw Empty)
  putStrLn (draw (Across 9))
  putStrLn (draw (DownAcross 9 9))
  putStrLn (draw v)
  putStrLn (draw (vv [1, 3, 5, 9]))
  putStrLn (drawGrid grid1)
  putStrLn (drawGrid (solver grid1))

