import Data.List (sort)
import Data.Maybe (mapMaybe)
import Control.Monad (replicateM)

getWords :: Read a => IO [a]
getWords = (map read . words) <$> getLine
{-# INLINE getWords #-}

main :: IO ()
main = do
    t <- read <$> getLine
    mapM_ (\i -> putStr ("Case #" ++ show i ++ ": ") >> parser) ([1..t] :: [Int])

parser :: IO ()
parser = do
    -- > One line with two integers N: the number of ingredients, and P, the number of packages of each ingredient.
    [n, _ {- p -}] <- getWords

    -- > One line with N integers Ri.
    -- > The i-th of these represents the number of grams of the i-th ingredient needed to make one serving of ratatouille.
    recipe <- getWords -- n items

    -- > N more lines of P integers each.
    -- > The j-th value on the i-th of these lines, Qij, represents the quantity, in grams, in the j-th package of the i-th ingredient.
    ingredients <- replicateM n getWords -- n * p

    print . countKits $ servingRange recipe ingredients


-- Returns sorted list of ranges which number of serving can be in.
servingRange :: [Int] {- recipe -} -> [[Int]] {- ingredients -} -> [[(Int, Int)]]
{- This function sorts the ranges.
   Check minimum first, if both are equal, check range size.
   That means: Check minimum first, and then check maximum if both are equal.
   Tuple's Ord instance is like this:

     deriving instance (Ord a, Ord b) => Ord (a, b)

   Order of deriving typeclass is left-to-right.
   That means: we don't need to write new sort function, because Haskell already does.
-}
servingRange = zipWith (\r -> sort . mapMaybe (serve r))
   where serve r q = if lower > upper then Nothing else Just res 
        {-
          let q = a single package's quantity, in grams.
              r = the grams which recipe wants of `q` ingredient.
              x = servings of `q` package, in integer.

          then should be:
              x * r * 0.9 <= q <= x * r * 1.1

          so:
              q / 1.1r <= x <= q / 0.9r

          x is integer:
              ceil(q/1.1r) <= x <= floor(q/0.9r)
        -}
          where qr = (fromIntegral q / fromIntegral r) :: Double
                res@(lower, upper) = (ceiling (qr/1.1), floor (qr/0.9))

-- https://twitter.com/nameEO/status/853227412267573248
behead :: [a] -> Maybe (a, [a])
behead (x:xs) = Just (x, xs)
behead [] = Nothing

andRange :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
andRange (min1, max1) (min2, max2) = (max min1 min2, min max1 max2)

countKits :: [[(Int, Int)]] -> Int
countKits = count 0
   where count i xs = case unzip <$> mapM behead xs of
            Just (heads, tails) ->
                let (mn, mx) = foldr1 andRange heads
                in if mn > mx
                    then count i $ map (dropWhile (\(_, m) -> mn > m)) xs -- drop while range's maximum is lower than any of heads' minimum
                    else count (succ i) tails
            Nothing -> i
