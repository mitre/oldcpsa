module CPSA.Lib.State where

import Data.Maybe (catMaybes)
import CPSA.Lib.Algebra
import CPSA.Lib.Utilities

-- A transition
newtype Tran t = Tran (Maybe t, Maybe t, Maybe t) deriving (Show, Eq, Ord)

now :: Tran t -> Maybe t
now (Tran (t, _, _)) = t

next :: Tran t -> Maybe t
next (Tran (_, t, _)) = t

-- Labels are ignored
-- label :: Tran t -> Maybe t
-- label (Tran (_, _, t)) = t

observer :: Tran t -> Bool
observer (Tran (_, Nothing, _)) = True
observer _ = False

path :: Tran t -> Bool
path x = not (observer x)

transition :: Tran t -> Bool
transition (Tran (Just _, Just _, _)) = True
transition _ = False

tranTerms :: Tran t -> [t]
tranTerms (Tran (Nothing, Nothing, _)) =
  assertError ("Encountered sync with no pre- or post-state in State.hs:tranTerms")
tranTerms (Tran (now, next, label)) =
  catMaybes [now, next, label]

mapTran :: (t -> t) -> Tran t -> Tran t
mapTran f (Tran (now, next, label)) =
  Tran (maybe Nothing (Just . f) now, maybe Nothing (Just . f) next,
        maybe Nothing (Just . f) label)

matchTran :: Algebra t p g s e c => Tran t -> Tran t -> (g, e) -> [(g, e)]
matchTran (Tran (c, n, l)) (Tran (c', n', l')) ge =
  do
    ge0 <- matchNext c c' ge
    ge1 <- matchNext n n' ge0
    matchNext l l' ge1

matchNext :: Algebra t p g s e c => Maybe t -> Maybe t -> (g, e) -> [(g, e)]
matchNext Nothing Nothing ge = [ge]
matchNext (Just t) (Just t') ge = match t t' ge
matchNext _ _ _ = []

unifyTran :: Algebra t p g s e c => Tran t -> Tran t -> (g, s) -> [(g, s)]
unifyTran (Tran (c, n, l)) (Tran (c', n', l')) gs =
  do
    gs0 <- unifyNext c c' gs
    gs1 <- unifyNext n n' gs0
    unifyNext l l' gs1

unifyNext :: Algebra t p g s e c => Maybe t -> Maybe t -> (g, s) -> [(g, s)]
unifyNext Nothing Nothing ge = [ge]
unifyNext (Just t) (Just t') ge = unify t t' ge
unifyNext _ _ _ = []
