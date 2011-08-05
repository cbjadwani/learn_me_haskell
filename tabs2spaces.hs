import System.IO
import System.Environment
import Control.Monad

main :: IO ()
main    = do
            args <- getArgs
            when ((length args) /= 3) $ do
                putStrLn "usage: tabs2spaces <tabsize> <infile> <outfile>"
                fail "Invalid num of args"
            inh <- openFile (args !! 1) ReadMode
            outh <- openFile (args !! 2) WriteMode
            inpStr <- hGetContents inh
            let n = read $ args !! 0 :: Int
                result = processData n inpStr
            hPutStr outh result
            hClose inh
            hClose outh

processData n inp = unlines $ map (tab2spaces n 0) $ lines inp

tab2spaces n col []     = []
tab2spaces n col (c:cs) = if c /= '\t' then
                            c:(tab2spaces n (col+1) cs)
                            else
                              let n' = n - (col `mod` n)
                                  times n x = take n $ repeat x
                              in (n' `times` ' ') ++ (tab2spaces n (col+n') cs)
