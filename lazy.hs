import System.Environment (getArgs)

data Expr = S | K | I | Iota | A Expr Expr
    | CInt Int | CSucc | CList [Expr]

eval :: Expr -> Expr
eval (A x y) = apply (eval x) $ eval y
    where
        apply :: Expr -> Expr -> Expr
        apply (A (A S x) y) z = apply (apply x z) $ apply y z
        apply (A K x) y = x
        apply I x = x
        apply Iota x = apply (apply x S) K
        apply (A (CInt i) x) y = iterate (apply x) y !! i
        apply CSucc (CInt i) = CInt $ i + 1
        apply (CList (x : t)) y = apply (apply y x) $ CList t
        apply x y = A x y
eval x = x

parse :: [Char] -> Expr
parse [] = I
parse x = foldl1 A $ map fromIota $ parse' $ tail $ scanl f (' ', 0) $ filter (`elem` "()*01IKS`iks") x
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
            | x == '`' = (\ (x : y : z) -> A (fromIota x) (fromIota y) : z) $ parse' t
            | x == '*' = (\ (x : y : z) -> A x y : z) $ parse' t
            | x == '(' = (\ (x, y) -> foldl1 A (fromIota <$> parse' x) : parse' (tail y)) $ span ((>= i) . snd) t
            where
                g :: Expr -> (Char, Int) -> Expr
                g x ('0', _) = A (A x S) K
                g x _ = A S $ A K x
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
        uncInt x = uncInt $ eval $ A (A x CSucc) $ CInt 0

        uncList :: Expr -> [Expr]
        uncList (CList l) = l
        uncList x = eval (A x K) : uncList (eval $ A x $ A S K)

main :: IO ()
main = do
    [path] <- getArgs
    code <- readFile path
    input <- getContents
    putStr $ unchurch $ A (parse code) $ church input
