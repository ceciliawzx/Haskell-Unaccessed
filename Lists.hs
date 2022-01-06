import Data.Char ( isAlpha, isSpace )
import Data.List ( (\\), inits, tails )

pos :: Char -> String -> Int
pos c (s : ss)
  | c == s    = 0
  | otherwise = 1 + pos c ss
pos _ _
  = error "empty list"

twoSame :: [Int] -> Bool
twoSame (x : xs) 
  = x `elem` xs || twoSame xs
twoSame _
  = False 

-- rev :: [a] -> [a]
-- rev (x : xs) = rev xs ++ [x]
-- rev _
--   = []

rev :: [a] -> [a]
rev list@(x : xs)
  = rev' list []
    where rev' (x : xs) res = rev' xs (x : res)
          rev' _ res = res

substring :: String -> String -> Bool
substring s str@(x : xs) = s `elem` list
  where list = map (take n) suff
        suff = take l (iterate tail str)
        l = length str
        n = length s

transpose :: String -> String -> String -> String 
transpose ori@(s : ss) aft str
  = transpose' aft ""
    where transpose' :: String -> String -> String
          transpose' aft@(x : xs) res = transpose' xs (res ++ [str !! n])
            where n = pos x ori
          transpose' _ res = res

removeWhitespace :: String -> String
removeWhitespace str@(s : ss)
  | isSpace s = removeWhitespace ss
  | otherwise = str
removeWhitespace _ = []

nextword :: String -> (String, String)
nextword (c : cs)
  | isSpace c   = ("", cs)
  | otherwise   = (c : word, rest)
    where (word, rest) = nextword cs
nextword _
  = ("", "")

splitUp :: String -> [String]
splitUp str@(c : cs)
  = w : splitUp rest
    where (w, rest) = nextword (removeWhitespace str)
splitUp _o
  = []

primeFactors :: Int -> [Int]
primeFactors
  = factor 2
    where factor :: Int -> Int -> [Int]
          factor p 1
            = []
          factor p n
            | n `mod` p == 0   = p : factor p (n `div` p)
            | otherwise        = factor (p + 1) n

hcf :: Int -> Int -> Int
hcf a b
  = product com
    where ps1 = primeFactors a
          ps2 = primeFactors b
          com = ps1 \\ (ps1 \\ ps2)

lcm1 :: Int -> Int -> Int
lcm1 a b
  = min a b * product (ps2 \\ ps1)
    where ps1 = primeFactors (min a b)
          ps2 = primeFactors (max a b)

-- 3.2 List comprehension

findAll :: Eq a => a -> [(a, b)] -> [b]
findAll a t = [y | (x, y) <- t, x == a]
          
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove a t = [p | p@(x, _) <- t, x /= a]
-- filter ((/=5).fst) [(5,3),(4,6),(5,9)]

quicksort :: [Int] -> [Int]
quicksort (c : cs) = less ++ [c] ++ more
  where less = quicksort [x | x <- cs, x <= c]
        more = quicksort [y | y <- cs, y > c]
quicksort _ = []

allSplits :: [a] -> [([a], [a])]
allSplits list
  = [splitAt n list | n <- [1..length list - 1]]

-- prefixes :: [t] -> [[t]]
-- prefixes list
--   = [take n list | n <- [1..length list]]

prefixes :: [t] -> [[t]]
prefixes list@(c : cs)
  = [c] : [c : ps | ps <- prefixes cs]
prefixes _
  = []

-- substrings :: String -> [String]
-- substrings list@(c : cs)
--   = substrings' list ++ substrings cs
--     where substrings' list@(s : ss)
--             = [s] : [s : ps | ps <- substrings' ss]
--           substrings' _
--             = []
-- substrings _
--   = []

substrings :: String -> [String]
substrings list
  = [s | t <- tails list, s <- tail (inits t)]

perms :: [Int] -> [[Int]]
perms []
  = [[]]
perms list
  = [c : ps | c <- list, ps <- perms (list \\ [c])]

routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes ori des map
  = routes' ori []
    where routes' ori been
           | ori `elem` been  = []
           | ori == des       = [[ori]]
           | otherwise        = [ori : des' | ori' <- [y | (x, y) <- map, x == ori], des' <- routes' ori' (ori : been)]