 module Main where
 
  import System.Environment
  import Data.Char
  import Data.List
  import Text.ParserCombinators.Parsec
 type Entry    = (String, String)
  type Section  = (String, [Entry])
 type IniData  = [Section]
inidata = spaces >> many section >>= return
section = do 
          char '['
          name <- ident
          char ']'
          stringSpaces
          char '\n'
          spaces
          el <- many entry
          return (name, el)
 entry = do 
          k <- ident
          stringSpaces
          char '=' 
          stringSpaces
          v <- value
          spaces
          return (k, v)
 ident = many1 (letter <|> digit <|> oneOf "_.,:(){}-#@&*|") >>= return . trim value = many (noneOf "\n") >>= return . trim
stringSpaces = many (char ' ' <|> char '\t')
 trim = f . f
    where f = reverse . dropWhile isSpace
split delim = foldr f [[]]
42   where
43     f x rest@(r:rs)
44       | x == delim  = [delim] : rest
45       | otherwise   = (x : r) : rs
findValue ini s p = do
52             el <- find (\x->fst x == s) ini
53             v  <- find (\x->fst x == p) (snd el)
54             return $ snd $ v
56 main = do
57       args  <- getArgs
58       prog  <- getProgName
59       if (length args) /= 3 
60          then putStrLn $ "Usage: " ++ prog ++ " <file.ini> <section>
else do 
62             file  <- readFile $ head args
63             [s,p] <- return $ tail args
64             lns   <- return ( removeComments file )
65             case (parse inidata "some text" lns) of
66                Left  err -> putStr "Parse error: " >> print err
67                Right x   -> case (findValue x s p) of
68                                 Just x -> putStrLn x
69                                 Nothing-> putStrLn "Can't find requested parameter"
70             return ()