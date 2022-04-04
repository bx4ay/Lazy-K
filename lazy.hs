import System.Environment (getArgs)
import System.IO (hSetBinaryMode, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

data Expr = S | K | I | Iota | N | P | CI Int | CS | A Expr Expr

a :: Expr -> Expr -> Expr
a (A (A S x) y) z = a (a x z) $ a y z
a (A K x) _ = x
a I x = x
a Iota x = a (a x S) K
a (A N _) x = x
a (A (A P x) y) z = a (a z x) y
a (A (CI i) x) y = iterate (a x) y !! i
a CS (CI i) = CI $ i + 1
a x y = A x y

parse :: [Char] -> Expr
parse [] = I
parse x = foldl1 a $ fromIota <$> parse' (tail $ scanl f (' ', 0) $ filter (`elem` "()*01IKS`iks") $ concat $ takeWhile (/= '#') <$> lines x)
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
            | x == '`' = (\ (x : y : t) -> a (fromIota x) (fromIota y) : t) $ parse' t
            | x == '*' = (\ (x : y : t) -> a x y : t) $ parse' t
            | x == '(' = (\ (x, y) -> foldl1 a (fromIota <$> parse' x) : parse' (tail y)) $ span ((>= i) . snd) t
            where
                g :: Expr -> (Char, Int) -> Expr
                g x ('0', _) = a (a x S) K
                g x _ = A S $ A K x
        parse' _ = []

        f :: (Char, Int) -> Char -> (Char, Int)
        f (_, i) x = (x, i + fromEnum (x == '(') - fromEnum (x == ')'))

church :: [Char] -> Expr
church = cL . map CI . (++ repeat 256) . map fromEnum
    where
        cL :: [Expr] -> Expr
        cL = foldr (A . A P) N

unchurch :: Expr -> [Char]
unchurch = map toEnum . takeWhile (< 256) . map uncI . uncL
    where
        uncI :: Expr -> Int
        uncI (CI i) = i
        uncI x = uncI $ a (a x CS) $ CI 0

        uncL :: Expr -> [Expr]
        uncL x = a x K : uncL (a x N)

main :: IO ()
main = do
    args <- getArgs
    (b, codes) <- f args
    hSetBinaryMode stdin b
    hSetBinaryMode stdout b
    hSetBuffering stdout NoBuffering
    input <- getContents
    putStr $ unchurch $ foldl1 (flip a) $ church input : map parse codes
    where
        f :: [[Char]] -> IO (Bool, [[Char]])
        f ("-b" : x) = sequence (True, sequence $ g x)
        f x = sequence (False, sequence $ g x)

        g :: [[Char]] -> [IO [Char]]
        g ("-e" : x : t) = pure x : if null t then [] else g t
        g (x : t) = readFile x : if null t then [] else g t
        g _ = [getLine]
