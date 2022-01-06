addDigit :: Int -> Int -> Int 
addDigit m n = m * 10 + n

convert :: Float -> Float
convert t = t / (5/9) + 32

type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea x y z = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2
        a = distance x y
        b = distance x z
        c = distance y z

isPrime :: Int -> Bool
isPrime n = 0 `notElem` [mod n i | i <- [2..n-1]]

fact :: Int -> Int
fact n
  | n == 0    = 1
  | otherwise = n * fact (n - 1)

perm :: Int -> Int -> Int
perm n r
  | r == 0    = 1
  | otherwise = (n - r + 1) * perm n (r - 1)

choose :: Int -> Int -> Int
choose n r
  | n == r    = 1
  | otherwise = choose (n - 1) r * (n `div` (n - r))

remainder :: Int -> Int -> Int 
remainder m n
  | m < n     = m
  | otherwise = remainder (m - n) n

quotient :: Int -> Int -> Int 
quotient m n
  | m < n     = 0
  | otherwise = 1 + quotient (m - n) n

binary :: Int -> Int
binary n
  | n < 2     = n
  | otherwise = addDigit (binary q) r
    where q = quotient n 2 
          r = remainder n 2

changeBase :: Int -> Int -> Int 
changeBase n b
  | n < b     = n
  | otherwise = addDigit (changeBase q b) r
    where q = quotient n b
          r = remainder n b

-- precon: m, n >= 0
add :: Int -> Int -> Int 
add m n 
  | n == 0    = m
  | otherwise = add (succ m) (pred n)

larger :: Int -> Int -> Int 
larger m n = larger' m n 0
  where larger' a b res
          | a < 0     = res + b
          | b < 0     = res + a
          | otherwise = larger' (pred a) (pred b) (succ res)

chop :: Int -> (Int, Int)
chop n
  | n < 10    = (0, n)
  | otherwise = (1 + q, r)
    where (q, r) = chop (n - 10)

concatenate :: Int -> Int -> Int 
concatenate m n
  | n == 0    = m
  | otherwise = addDigit (concatenate m q) r
    where (q, r) = chop n

fib :: Int -> Int 
fib n
  = fib' 0 1 n
    where fib' :: Int -> Int -> Int -> Int
          fib' f1 f2 n
            | n == 0    = f1
            | otherwise = fib' f2 (f1 + f2) (n - 1)

-- fib :: Int -> Int
-- fib n
--   | n == 0    = 0
--   | n == 1    = 1
--   | otherwise = fib (n - 1) + fib (n + 1)

goldenRatio :: Float -> Float
goldenRatio e
  = golden' 1 2 1
    where golden' :: Int -> Int -> Float -> Float
          golden' f1 f2 r
            | abs (r - r') < e = r'
            | otherwise        = golden' f2 (f1 + f2) r'
              where r' =  fromIntegral f2 / fromIntegral f1

goldenRatio' :: Float -> Float
goldenRatio' e
  = gr 1 2 1
    where
    gr f f' r
      | abs ((r - r') / r) < e = r'
      | otherwise = gr f' (f + f') r'
        where
        r' = fromIntegral f' / fromIntegral f