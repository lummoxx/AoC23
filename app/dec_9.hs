module Dec_9 where
main = do
    l <- map (map read . words) . lines <$> readFile "9th"
    print $ map sum [ (map (extr (+) last) l)
                    , (map (extr (-) head) l)]
  where extr f g = foldr f 0 . go where go l | all (== head l) l = [g l]
                                             | True = g l : go (zipWith (-) (tail l) l)