-- name:
-- id:
-- acknowledgements:

import System.Environment
import Graphics.Gloss

type State = [[Int]]

box :: Float -> Float -> Color -> Picture
box x y c =
  Color c (Polygon [(x * 50,      y * 50),
                    (x * 50 + 45, y * 50),
                    (x * 50 + 45, y * 50 + 45),
                    (x * 50,      y * 50 + 45)])

-- fromIntegral 转int类型
draw :: State -> Picture
draw grid =
  Pictures [ box x y (if k == 1 then red else if k == 0 then blue else if k == 2 then yellow else black)
           | (i, row) <- zip [0..] grid,
             (j, k)  <- zip [0..] row,
             let x = fromIntegral j,
             let y = fromIntegral (-i)
           ]









-- >>> prepareData [[1,2,3], [4,5,6], [7,8,9]]
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( C:\haskell_workspace\testtest.hs, interpreted )
-- <BLANKLINE>
-- C:\haskell_workspace\testtest.hs:6:1-21: error:
--     Could not find module eGraphics.Glossf
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- Failed, no modules loaded.
--
