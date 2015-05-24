-- $ echo "1 + 1" | ./Optional


-- 文字列を整数に変換。できなければ無効
toNum :: String -> Maybe Int

toNum s = case reads s of
              [(n,"")] -> Just n
              _        -> Nothing


-- 四則演算。演算できなければ無効
addOp :: Int -> Int -> Maybe Int

addOp a b = Just (a + b)

subOp :: Int -> Int -> Maybe Int

subOp a b = Just (a - b)

mulOp :: Int -> Int -> Maybe Int

mulOp a b = Just (a * b)

divOp :: Int -> Int -> Maybe Int

divOp _ 0 = Nothing

divOp a b = Just (a `div` b)


-- "+", "-", "*", "/"のどれかの文字列を演算に変換。それ以外は無効
toBinOp :: String -> Maybe (Int -> Int -> Maybe Int)

toBinOp "+" = Just addOp

toBinOp "-" = Just subOp

toBinOp "*" = Just mulOp

toBinOp "/" = Just divOp

toBinOp _ = Nothing


eval :: String -> Maybe Int

eval expr = do

    -- スペースで分裂する、３つに分裂できなければ無効
    -- "1 + 2" -> "1" "+" "2"
    let [ sa, sop, sb ] = words expr

    a <- toNum sa      --文字列を数値に変換

    op <- toBinOp sop  --文字列を演算に変換

    b <- toNum sb      --文字列を数値に変換

    a `op` b           --数値 演算 数値 の計算


main :: IO ()

main = getLine >>= putStrLn . maybe "invalid" show . eval















