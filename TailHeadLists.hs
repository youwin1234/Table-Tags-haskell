module TailHeadLists where
-- Tristan Berger
-- hawkid = teberger

{- fill in the two constructors Nil and Cons for this data type -}
data ThList a = Nil | Cons (ThList a) a deriving Show

{- thappend is like ++ -}
thappend :: ThList a -> ThList a -> ThList a
thappend Nil secs = secs
thappend (Cons Nil ns) secs = (Cons secs ns)
thappend (Cons tl ns) secs = (Cons (thappend tl secs) ns)

{- thlength is like length, but our type is specific to lists not "Foldables" -}
thlength :: ThList a -> Int
thlength Nil = 0
thlength (Cons li _) = 1 + (thlength li)

{- thmap is like map -}
thmap ::  (a -> b) -> ThList a -> ThList b
thmap x Nil = Nil
thmap x (Cons li z) = (Cons (thmap (x) li) (x z))

{- thfilter is like filter -}
thfilter :: (a -> Bool) -> ThList a -> ThList a
thfilter _ Nil = Nil
thfilter x (Cons li z) | (x z) = (Cons (thfilter x li) z)
                                 | otherwise = (thfilter x li)

{- thfoldl is like foldl, but our type is specific to lists not "Foldables" -}
thfoldl :: (b -> a -> b) -> b -> ThList a -> b
thfoldl _ results Nil = results
thfoldl x results (Cons li z) = thfoldl x (x result z) li
{- thintersperse is like intersperse
note that if you want to try out intersperse you need to
import Data.List
-}
thintersperse :: a -> ThList a -> ThList a
thintersperse _ Nil = Nil
thintersperse _ (Cons Nil z) = (Cons Nil z)
thintersperse x (Cons li z) = (Cons (Cons (thintersperse x li) x) z)

{- thconcat is like concat -}
thconcat :: ThList (ThList z) -> ThList z
thconcat Nil = Nil
thconcat (Cons li Nil) = thconcat li
thconcat (Cons li (Cons li1 z)) = thappend (Cons li1 z) (thconcat li)
