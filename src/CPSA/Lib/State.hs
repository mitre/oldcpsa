module CPSA.Lib.State where

import Data.Maybe (catMaybes)
import CPSA.Lib.Algebra
import CPSA.Lib.Utilities

-- A transition (pre-state, post-state)
newtype Tran t = Tran (Maybe t, Maybe t) deriving (Show, Eq, Ord)

now :: Tran t -> Maybe t
now (Tran (t, _)) = t

next :: Tran t -> Maybe t
next (Tran (_, t)) = t

observer :: Tran t -> Bool
observer (Tran (_, Nothing)) = True
observer _ = False

path :: Tran t -> Bool
path x = not (observer x)

transition :: Tran t -> Bool
transition (Tran (Just _, Just _)) = True
transition _ = False

tranTerms :: Tran t -> [t]
tranTerms (Tran (Nothing, Nothing)) =
  assertError ("Encountered sync with no pre- or post-state in State.hs:tranTerms")
tranTerms (Tran (now, next)) =
  catMaybes [now, next]

mapTran :: (t -> t) -> Tran t -> Tran t
mapTran f (Tran (now, next)) =
  Tran (maybe Nothing (Just . f) now, maybe Nothing (Just . f) next)

matchTran :: Algebra t p g s e c => Tran t -> Tran t -> (g, e) -> [(g, e)]
matchTran (Tran (c, n)) (Tran (c', n')) ge =
  do
    ge <- matchNext c c' ge
    matchNext n n' ge

matchNext :: Algebra t p g s e c => Maybe t -> Maybe t -> (g, e) -> [(g, e)]
matchNext Nothing Nothing ge = [ge]
matchNext (Just t) (Just t') ge = match t t' ge
matchNext _ _ _ = []

unifyTran :: Algebra t p g s e c => Tran t -> Tran t -> (g, s) -> [(g, s)]
unifyTran (Tran (c, n)) (Tran (c', n')) gs =
  do
    gs <- unifyNext c c' gs
    unifyNext n n' gs

unifyNext :: Algebra t p g s e c => Maybe t -> Maybe t -> (g, s) -> [(g, s)]
unifyNext Nothing Nothing ge = [ge]
unifyNext (Just t) (Just t') ge = unify t t' ge
unifyNext _ _ _ = []
