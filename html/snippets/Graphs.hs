brackets :: String -> [Int]
brackets expr = foldl pda [0] expr
  where
    pda []    _ = []
    pda (x:s) c = delta c x ++ s

    delta '(' 0 = [1,0]
    delta '(' 1 = [1,1]
    delta ')' _ = []
          
