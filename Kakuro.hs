
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

main = do
  putStrLn "---"

