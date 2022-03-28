import System.Environment (getArgs)
import System.IO (hSetBinaryMode, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

data Expr = S | K | I | Iota | CInt Int | CSucc | CList [Expr] | A Expr Expr

app :: Expr -> Expr -> Expr
app (A (A S x) y) z = app (app x z) $ app y z
app (A K x) y = x
app I x = x
app Iota x = app (app x S) K
app (A (CInt i) x) y = iterate (app x) y !! i
app CSucc (CInt i) = CInt $ i + 1
app (CList (x : t)) y = app (app y x) $ CList t
app x y = A x y

parse :: [Char] -> Expr
parse [] = I
parse x = foldl1 app $ map fromIota $ parse' $ tail $ scanl f (' ', 0) $ filter (`elem` "()*01IKS`iks") x
    where
        fromIota :: Expr -> Expr
        fromIota Iota = I
        fromIota x = x

        parse' :: [(Char, Int)] -> [Expr]
        parse' ((x, i) : t)
            | x == 'I' = I : parse' t
            | x == 'i' = Iota : parse' t
            | x `elem` "Kk" = K : parse' t
            | x `elem` "Ss" = S : parse' t
            | x `elem` "01" = (\ (x, y) -> foldl g I x : parse' y) $ span ((`elem` "01") . fst) $ (x, i) : t
            | x == '`' = (\ (x : y : t) -> app (fromIota x) (fromIota y) : t) $ parse' t
            | x == '*' = (\ (x : y : t) -> app x y : t) $ parse' t
            | x == '(' = (\ (x, y) -> foldl1 app (fromIota <$> parse' x) : parse' (tail y)) $ span ((>= i) . snd) t
            where
                g :: Expr -> (Char, Int) -> Expr
                g x ('0', _) = app (app x S) K
                g x _ = app S $ app K x
        parse' _ = []

        f :: (Char, Int) -> Char -> (Char, Int)
        f (_, i) x = (x, i + fromEnum (x == '(') - fromEnum (x == ')'))

church :: [Char] -> Expr
church = CList . map CInt . (++ repeat 256) . map fromEnum

unchurch :: Expr -> [Char]
unchurch = map toEnum . takeWhile (< 256) . map uncInt . uncList
    where
        uncInt :: Expr -> Int
        uncInt (CInt i) = i
        uncInt x = uncInt $ app (app x CSucc) $ CInt 0

        uncList :: Expr -> [Expr]
        uncList (CList l) = l
        uncList x = app x K : uncList (app x $ app S K)

main :: IO ()
main = do
    args <- getArgs
    (b, codes) <- f args
    hSetBinaryMode stdin b
    hSetBinaryMode stdout b
    hSetBuffering stdout NoBuffering
    input <- getContents
    putStr $ unchurch $ foldl1 (flip app) $ church input : map parse codes
    where
        f :: [[Char]] -> IO (Bool, [[Char]])
        f ("-b" : x) = sequence (True, sequence $ g x)
        f x = sequence (False, sequence $ g x)

        g :: [[Char]] -> [IO [Char]]
        g ("-e" : x : t) = pure x : if null t then [] else g t
        g (x : t) = readFile x : if null t then [] else g t
        g _ = [getLine]
