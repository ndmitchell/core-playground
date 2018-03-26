
infix 1 ===

eta f = \x -> f x
eta2 f = \x y -> f x y

x == y = primEq x y

proof1 f' x' = length (map f' x') === length x'

proof2 f' g' x' = map f' (map g' x') === map (f' . g') x'

proof3 f' = (($) . f') === eta2 f'

proof4 n x = take n (repeat x) === replicate n x

-- These aren't the same unless n is positive, as n=(-1) x=[1] is 1 vs _|_
-- proof5 n x = head (drop n x) === x !! n

proof6 f z g x = foldr f z (map g x) === foldr (f . g) z x

proof7 = (\x -> cycle [x]) === repeat

proof8 f x y = zipWith f (repeat x') === map (f x')

proof9 x y = (if x then False else y) === not x && y

proof10 = map fromJust . filter isJust === catMaybes

proof11 = mapMaybe id === catMaybes

proof12 f x = map f (repeat x) === repeat (f x)

proof13 f x y = map (uncurry f) (zip x y) === zipWith f x y

-- Results in non-termination trying to prove iterate (id*) = repeat, for increasingly more id's
-- proof14 = iterate id === repeat

proof15 f x = catMaybes (map f x) === mapMaybe f x

proof16 = concatMap maybeToList === catMaybes

proof17 f g x = concatMap f (map g x) === concatMap (f . g) x

-- Results in non-termination because you are building up progressively bigger (:) accumulator
-- proof18 x = head (reverse x) === last x
