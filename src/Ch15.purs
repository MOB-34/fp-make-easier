module Ch15 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Foldable
import Data.Profunctor (class Profunctor, lcmap)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate pred) = Predicate (pred <<< f)

-- dimap :: ∀ (@p ∷ Type -> Type -> Type) (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type). Profunctor p ⇒ (a → b) → (c → d) → p b c → p a d

instance profunctorMoore :: Profunctor (Moore s) where
  dimap f g (Moore s extract step) = Moore s (g <<< extract) (\acc -> step acc <<< f)

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 extract step) l = extract $ foldl step s0 l

sizer :: Moore Int String Int
sizer = lcmap length addr

test :: Effect Unit
test = do
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]
  log $ show $ length <$> [ "This", "is", "the", "test" ]
-- log "------------------------------------"
-- log $ show $ runFoldL addr [ 1, 2, 3 ]
-- log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
-- log "------------------------------------"
-- log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
-- log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
-- log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
-- log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
-- log "------------------------------------"
-- log $ show $ runPredicate (Predicate odd) $ 10
-- log $ show $ runPredicate (Predicate odd) $ 11
-- log $ show $ odd 0
-- log $ show $ odd 1
