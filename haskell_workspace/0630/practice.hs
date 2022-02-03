records = [
(2000, [7.6, 6.0, 9.4, ..., 8.8]),
(2001, [4.9, 6.6, 9.8, ..., 8.4]),
(2002, [7.4, 7.9, 12.2, ..., 7.2]),
(2003, [5.5, 6.4, 8.7, ..., 9.2]),
(2004, [6.3, 8.5, 9.8, ..., 9.9]),
(2005, [6.1, 6.2, 9.0, ..., 6.4]),
(2006, [5.1, 6.7, 9.8, ..., 9.5]),
(2007, [7.6, 8.6, 10.8, ..., 9.0]),
(2008, [5.9, 5.5, 10.7, ..., 9.8]),
(2009, [6.8, 7.8, 10.0, ..., 9.0]),
(2010, [7.0, 6.5, 9.1, ..., 9.9]),
(2011, [5.1, 7.0, 8.1, ..., 7.5]),
(2012, [4.8, 5.4, 8.8, ..., 7.3]),
(2013, [5.5, 6.2, 12.1, ..., 8.3]),
(2014, [6.3, 5.9, 10.4, ..., 6.7]),
(2015, [5.8, 5.7, 10.3, ..., 9.3]),
(2016, [6.1, 7.2, 10.1, ..., 8.9]),
(2017, [5.8, 6.9, 8.5, ..., 6.6]) ]

cell :: Double -> String
cell x
   | x < 10 = "<td class='cold'>" ++ show x
   | x < 15 = "<td class='cool'>" ++ show x
   | x < 20 = "<td class='mild'>" ++ show x
   | x < 25 = "<td class='warm'>" ++ show x
   | otherwise = "<td class='hot'>" ++ show x


row :: (Int, [Double]) -> String
row (year, xs) =
    unlines ["<tr>", "<td>" ++ show year] ++
    [ cell x | x <- xs] ++ 
    ["</tr>"])


table :: String
table = 
    unlines (["<table>"] ++
    ["<tr>","<th>-"] ++ ["<th>" ++ show i | i <- [1..12]] ++
             [ row r | r <- records ] ++
             ["</table>"])

html :: String
html =
    "<!DOCTYPE html>" ++
    "<style>" ++
    "table {" ++
    "border-top: 1px solid;" ++
    "border-bottom: 1px solid;" ++
    "}" ++
    ".cold { background: #aaf; } /* -10 */" ++
    ".cool { background: #ccf; } /* 10-15 */" ++
    ".mild { background: #fef; } /* 15-20 */" ++
    ".warm { background: #fcc; } /* 20-25 */" ++
    ".hot { background: #faa; } /* 25- */" ++
    "</style>" ++
    "<h1>Temperatures in Tokyo</h1>" ++
    table

main :: IO ()
main = do
    putStrLn html
    file <- openFile "a.html" writeMode
    hPutStrLn file html
    hClose file