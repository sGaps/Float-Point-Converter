module Common (
    hasAtLeast,
    (!!?),
    zipWithMaybe
) where

-- | Verifies if a list has at least `n` elements.
hasAtLeast xs n = if n < 0 then True
                  else case xs !!? (n-1) of
                    Nothing -> False
                    _       -> True

-- TODO: convert `n` into `!n`
-- | Safe index function.
(!!?) :: Integral i => [a] -> i -> Maybe a
li !!? n = go li n
    where go (x:xs) 0 = Just x
          go (x:xs) k = go xs (k-1)
          go _      _ = Nothing

zipWithMaybe :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithMaybe mayfunc (x:xs) []     = mayfunc (Just x) Nothing  : zipWithMaybe mayfunc xs []
zipWithMaybe mayfunc []     (y:ys) = mayfunc Nothing (Just y)  : zipWithMaybe mayfunc [] ys
zipWithMaybe mayfunc (x:xs) (y:ys) = mayfunc (Just x) (Just y) : zipWithMaybe mayfunc xs ys
zipWithMaybe _       []     []     = []
