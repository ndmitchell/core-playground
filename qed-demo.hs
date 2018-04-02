
infix 1 ===

eta f = \x -> f x
eta2 f = \x y -> f x y

x == y = primEq x y

proof1 f' x' = length (map f' x') === length x'

proof2 f' g' x' = map f' (map g' x') === map (f' . g') x'

proof3 f' = (($) . f') === eta2 f'

proof4 n x = take n (repeat x) === replicate n x

drop' n xs = case compare n 0 of
    LT -> undefined
    EQ -> xs
    GT -> case xs of
                     [] -> []
                     x:xs -> drop' (n-1) xs

-- These aren't the same unless n is positive, as n=(-1) x=[1] is 1 vs _|_
-- proof5 n x = (if n < 0 then undefined else head (drop' n x)) === (if n < 0 then undefined else x !! n)
-- need to prove by keeping a store of which primitives have evaluated how
--

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
-- forall a. iterate id (id (id a))  ===  repeat a
-- Could be fixed by inling all id first, inlining the id when it arises,
-- or using the lemma id (id x) = id x

-- We can solve the simpler version though
proof14a = iterate (\x -> x) === repeat
-- proof14b = iterate (\x -> x) === iterate id


proof15 f x = catMaybes (map f x) === mapMaybe f x

proof16 = concatMap maybeToList === catMaybes

proof17 f g x = concatMap f (map g x) === concatMap (f . g) x

reverse2 xs = rev [] xs

reverse3          =  foldl (\x y -> y:x) []


rev acc xs = case xs of
    [] -> acc
    x:xs -> rev (x:acc) xs

-- Results in non-termination because you are building up progressively bigger flip (:) accumulator
-- proof18a x xs y = head (rev y (x:xs)) === last (x:xs)
--proof18 x = head (reverse2 x) === last x
-- need to make the generalisation argument (which can be done by hand)
-- then need to be able to see that something is an instance of it

plus a b = case a of
    Z -> b
    S a -> S (plus a b)

fibExp x = case x of
    Z -> Z
    S x -> case x of
        Z -> S Z
        S n -> fibExp (S n) `plus` fibExp n

fibLin' x a b = case x of
    Z -> b
    S n -> fibLin' n (a `plus` b) a

fibLin :: Nat -> Nat
fibLin n = fibLin' n (S Z) Z

-- proofFib d u = fibLin' d (fibExp (S u)) (fibExp u) === fibExp (d + u)
