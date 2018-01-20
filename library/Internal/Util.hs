module Internal.Util (
    optimalM
) where



--fillRatio = 0.5
optimalM :: Int -> Double -> Double -> Int
optimalM n fillRatio fpRatio = ceiling((fromIntegral n) / ((log fillRatio * log (1-fillRatio)) / (abs $ log fpRatio)))
