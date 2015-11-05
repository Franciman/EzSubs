module Subtitles where

import Text.Subtitles.SRT
import Control.Monad

newtype SubtitleMonad a = SubtitleMonad { runIt :: Subtitles -> a }


{-
Let's prove Functors' properties:

1) fmap id == id
 DIM.  fmap id (SubtitleMonad f) = SubtitleMonad (id . f) = SubtitleMonad f = id (SubtitleMonad f)

2) fmap (g . h) == fmap g . fmap h
 DIM. fmap (g . h) (SubtitleMonad f) = SubtitleMonad ( (g . h) . f ) = SubtitleMonad ( g . (h . f) ) = fmap g (SubtitleMonad (h . f)) = fmap g ( fmap h (SubtitleMonad f) ) = (fmap g . fmap h) (SubtitleMonad f)

-}
instance Functor SubtitleMonad where
  fmap g (SubtitleMonad f) = SubtitleMonad (g . f)

{-
Let's prove Applicatives' properties:

1) pure id <*> v = v
 DIM. pure id <*> (SubtitleMonad f)  = SubtitleMonad (\subs -> id) <*> (SubtitleMonad f) = SubtitleMonad (\subs' -> ((\subs -> id) subs') (f subs')) = SubtitleMonad (\subs' -> id (f subs')) = SubtitleMonad (\subs' -> f subs') = SubtitleMonad f

2) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
 DIM. pure (.) <*> SubtitleMonad f <*> SubtitleMonad g <*> SubtitleMonad h = SubtitleMonad (\subs -> (.)) <*> SubtitleMonad f <*> SubtitleMonad g <*> SubtitleMonad h = SubtitleMonad (\subs' -> ((\subs -> (.)) subs') (f subs')) <*> SubtitleMonad g <*> SubtitleMonad h =
                                                                           = SubtitleMonad (\subs' -> (.) (f subs')) <*> SubtitleMonad g <*> SubtitleMonad h = SubtitleMonad (\subs'' -> ((.) f subs'') (g subs '')) = SubtitleMonad (\subs'' -> f subs'' . g subs'') <*> SubtitleMonad h =
                                                                           = SubtitleMonad (\subs''' -> (f subs''' . g subs''')  (h subs''')) = SubtitleMonad (\subs''' -> f subs''' (g subs''' (h subs'''))) = SubtitleMonad f <*> ( SubtitleMonad (\subs''' -> (g subs''') (h subs'''))) =
                                                                           = SubtitleMonad f <*> ( SubtitleMonad g <*> SubtitleMonad h)

3) pure f <*> pure x = pure (f x)
 DIM. pure f <*> pure x = SubtitleMonad (const f) <*> SubtitleMonad (const x) = SubtitleMonad (\subs -> const f subs (const x subs)) = SubtitleMonad (\subs -> f x) = SubtitleMonad (const (f x)) = pure (f x) 

4) u <*> pure y = pure ($ y) <*> u
 DIM. (SubtitleMonad t) <*> pure y = (SubtitleMonad t) <*> (SubtitleMonad (const y)) = SubtitleMonad (\subs -> (t subs) (const y subs)) = SubtitleMonad (\subs -> t subs $ const y subs) = SubtitleMonad (\subs -> ($ (const y subs)) (t subs)) = SubtitleMonad (\subs -> ($ (const y subs)) (t subs)) =
                                   SubtitleMonad (\subs -> ($ y) (t subs)) = SubtitleMonad (\subs -> (const ($ y) subs) (t subs)) = (SubtitleMonad (const($ y)) <*> (SubtitleMonad t) = pure ($ y) <*> (SubtitleMonad t)
-}
instance Applicative SubtitleMonad where
  pure a = SubtitleMonad (const a)
  SubtitleMonad transformer <*> SubtitleMonad f = SubtitleMonad (\subs -> transformer subs (f subs))

-- TODO: Complete Monads' properties proofs

{-
Let's prove Monads' properties:

1) return a >>= k  =  k a
 DIM. return a >>= k = SubtitleMonad (\subs -> runIt (k (runIt (SubtitleMonad (const a))) subs) subs) = SubtitleMonad (\subs -> runIt (k ((const a) subs)) subs) = SubtitleMonad (\subs -> runIt (k a) subs) = SubtitleMonad (runIt (k a))
-}
instance Monad SubtitleMonad where
  sm >>= g = SubtitleMonad (\subs -> runIt ( g (runIt sm subs) ) subs)
