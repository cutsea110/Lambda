module Lambda where

--
-- ref. http://d.hatena.ne.jp/syamino/20120524
--

-- _if true "OK" "NG" => "OK"
-- _if false "OK" "NG" => "NG"
-- _if (_not true) 1 2 => 2
-- _if (_not false) 1 2 => 1
-- _if (_xor false false) 1 2 => 2
-- _if (_xor true false) 1 2 => 1
-- _if (_xor false true) 1 2 => 1
-- _if (_xor true true) 1 2 => 2
_if b t e = b t e
true  = \t f -> t
false = \t f -> f
_not m = \a b -> m b a
_xor m n = \a b -> m (n b a) (n a b)

-- _maybe 0 (*2) nothing => 0
-- _maybe 0 (*2) (just 4) => 8
_maybe d p m = m d p
nothing = \n j -> n
just  x = \n j -> j x

  
-- _either (+2) (*2) (left 3) => 5
-- _either (+2) (*2) (just 3) => 6
_either l r e = e l r
left  x = \l r -> l x
right y = \l r -> r y

-- _fst $ _snd (pair (pair 1 2) (pair 'a' 'b'))
-- _snd $ _fst (pair (pair 1 2) (pair 'a' 'b'))
pair x y = \p -> p x y
_fst p = p (\x y -> x)
_snd p = p (\x y -> y)


-- let lst = cons 1 $ cons 2 $ cons 3 $ cons 4 nil
-- _foldr 0 (+) lst
-- _foldr 1 (*) lst
nil = \c n -> n
cons h t = \c n -> c h (t c n)
_null l = l (\h t -> false) true
_head l = l (\h t -> h) undefined
_tail l = _fst (l (\x p -> pair (_snd p) (cons x (_snd p))) (pair nil nil))
_foldr f n l = l f n
_map f l = l (cons.f) nil -- _foldr (cons.f) nil l
_length l = _foldr (\x n -> 1+n) 0 l
_append xs ys = _foldr cons ys xs
_snoc x xs = _append xs (cons x nil)

-- couldn't determine type signature
-- nil = \c n -> n
-- cons x xs = \c n -> c x xs
-- _tail l = l nil (\x xs -> xs)
-- _head l = l const undefined
