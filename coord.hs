import Text.Printf


-- 座標の型
type Coord = (Double, Double)


-- 座標変換設定
data Config = Config { rotAt :: Coord            -- 回転中心座標
                       , theta :: Double         -- 回転量(ラジアン)
                       , ofs :: (Double, Double) -- 平行移動量
                     }


-- 座標変換関数の型は「座標を別の座標に移す」関数
type CoordConverter = Coord -> Coord


-- 平行移動のプリミティブ
trans :: (Double, Double) -> CoordConverter

trans (dx, dy) = \(x, y) -> (x + dx, y + dy)


-- 回転のプリミティブ
rotate :: Double -> CoordConverter

rotate t = \(x, y) -> (cos t * x - sin t * y, sin t * x + cos t * y)


-- 設定を元にした平行移動
transByConfig :: Config -> CoordConverter

transByConfig config = trans (ofs config)


-- 1,設定を元にした回転
rotateByConfig :: Config -> CoordConverter

rotateByConfig config = postTrans . rotate (theta config) . preTrans where
                            rotateAt = rotAt config
                            preTrans = trans (rotate pi $ rotateAt)
                            postTrans = trans rotateAt


-- 2,設定を元にした座標変換
convertByConfig :: Config -> CoordConverter

convertByConfig config = transByConfig config . rotateByConfig config


main :: IO ()

main = do
    -- (0.5, 0.5)を中心に反時計回りに45度回転させ、(-0.5)、(-0.5, -0.5)平行移動させる設定
    let config = Config { rotAt = (0.5, 0.5), theta = pi / 4, ofs = (-0.5, -0.5) }

    -- 変換前の座標、たとえばこの４点からなる正方形
    let unitRect = [(0, 0), (0, 1), (1, 1), (1, 0)]

    -- 変換後の座標
    let convertedRect = map (convertByConfig config) unitRect
    mapM_ (uncurry $ printf "%.6f, %.6f\n") convertedRect
















