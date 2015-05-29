import Text.Printf

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
         " " ++ concatMap (\ x -> if any (== x) values then show x else ".") [ 1..9 ]

main = do
  putStrLn (draw Empty)
  putStrLn (draw (Across 9))
  putStrLn (draw (DownAcross 9 9))
  putStrLn (draw v)
  putStrLn (draw (vv [1, 3, 5, 9]))

