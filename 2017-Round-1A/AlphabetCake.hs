import Control.Monad (replicateM)

getWords :: Read a => IO [a]
getWords = (map read . words) <$> getLine
{-# INLINE getWords #-}

main :: IO ()
main = do
    t <- read <$> getLine
    -- > For each test case, output one line containing Case #x: and nothing else.
    -- > Then output R more lines of C characters each.
    mapM_ (\i -> putStr ("Case #" ++ show i ++ ":\n") >> parser) ([1..t] :: [Int])

parser :: IO ()
parser = do
    -- Each begins with one line with two integers R and C.
    [r, _ {- c -}] <- getWords

    -- > Then, there are R more lines of C characters each, representing the cake.
    -- > Each character is either
    -- >   an uppercase English letter (which means that your assistant has already added that letter to that cell)
    -- >   or ? (which means that that cell is blank).
    grid <- replicateM r getLine

    -- > Output R more lines of C characters each.
    -- > Your output grid must be identical to the input grid, but with every ? replaced with an uppercase English letter,
    -- > representing that that cell appears in the slice for the child who has that initial.
    mapM_ putStrLn $ fillGrid grid

{-
  How this function works:
    0. Receive problem set

        ?????????
        ???A?????
        ??????B??
        ?????????
        ???C???D?
        ?????????

    1. Fill row first
      1-1. Fill to left

            ?????????
            AAAA?????
            BBBBBBB??
            ?????????
            CCCCDDDD?
            ?????????

      1-2. Fill to right

            ?????????
            AAAAAAAAA
            BBBBBBBBB
            ?????????
            CCCCDDDDD
            ?????????

    2. Fill column next
      2-1. Fill to up

            AAAAAAAAA
            AAAAAAAAA
            BBBBBBBBB
            CCCCDDDDD
            CCCCDDDDD
            ?????????

      2-2. Fill to down

            AAAAAAAAA
            AAAAAAAAA
            BBBBBBBBB
            CCCCDDDDD
            CCCCDDDDD
            CCCCDDDDD

  yeah this is unfair but it works

  * Assume that each of the initials which assistant wrote is unique.
-}
fillGrid :: [String] -> [String]
fillGrid = extend ((== '?') . head) "?" {- columns -} . map (extend (== '?') '?') {- rows -}
  where extend func def xs =
        -- func: A function checks if it has `?` or not
        -- def: Replace unresolved `?` to this.
            let (questions, right) = span func xs -- get until `?` does not appear
            in case right of
                [] -> map (const def) xs -- all of these were `?`
                (r:rs) -> map (const r) questions {- fill up -} ++ r : extend func r rs
