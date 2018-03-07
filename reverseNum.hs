
sizeofInt :: Int -> Int
sizeofInt x = if m == 0 then 1
  else 1 + sizeofInt m
  where m = x `div` 10

-- doesn't print leading 0
reverseNum :: Int -> Int
reverseNum x = go x 0 (sizeofInt x)
  where go 0 acc n = acc
        go x acc n = go q (acc + r * p) (n - 1)
          where q = x `div` 10
                r = x `rem` 10
                p = 10 ^ (n - 1)
