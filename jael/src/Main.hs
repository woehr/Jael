import Jael.Compile

main :: IO ()
main = hGetContents stdin
         >>= \x -> putStrLn $ either tshow id $ compile (pack x)

