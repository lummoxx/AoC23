module Dec_9 where
main = do
    ls <- map (map read . words) . lines <$> readFile "9th"
    print $ map sum [ (map (extr (+) last) ls)
                    , (map (extr (-) head) ls)]
  where extr f g = foldr f 0 . go where go l | all (== head l) l = [g l]
                                             | True = g l : go (zipWith (-) (tail l) l)