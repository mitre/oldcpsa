module CPSA.Lib.AlgebraLibrary where

import CPSA.Lib.Utilities
import CPSA.Lib.Algebra

{--
import CPSA.Lib.Debug
--}

-- Useful operations on variables

-- Are the vars in ts a subset of the ones in ts'?
varSubset :: Algebra t p g s e c => [t] -> [t] -> Bool
varSubset ts ts' =
    all (flip elem (varsInTerms ts')) (varsInTerms ts)

varsInTerms :: Algebra t p g s e c => [t] -> [t]
varsInTerms ts =
    foldl addVars [] ts

addVars :: Algebra t p g s e c => [t] -> t -> [t]
addVars ts t = foldVars (flip adjoin) ts t

matchMany :: Algebra t p g s e c => [t] -> [t] -> (g,e) -> [(g,e)]
matchMany [] [] ge = [ge]
matchMany [] _ _ = []
matchMany _ [] _ = []
matchMany (t:ts) (t':ts') ge = concatMap (matchMany ts ts') (match t t' ge)

matchMany_v :: Algebra t p g s e c => [t] -> [t] -> (g,e) -> [(g,e)]
matchMany_v [] [] ge = [ge]
matchMany_v [] _ _ = []
matchMany_v _ [] _ = []
matchMany_v (t:ts) (t':ts') ge = concatMap (matchMany_v ts ts') (match t t' ge)

-- stripExp x t: apply the map (x -> 1) to t and return the result.
stripExp :: Algebra t p g s e c => t -> t -> t
stripExp x t
  | (not $ isNum x) = t
  | otherwise = case destroyer x of
    Nothing -> t
    Just s -> substitute s t

-- zeroIndicator: zero out the indicator of all restricted variables in t.
zeroIndicator :: Algebra t p g s e c => t -> (g,e) -> [t] -> [(g,e)]
zeroIndicator _ ge [] = [ge]
zeroIndicator t ge (v:vs) =
  concatMap (\ge -> zeroIndicator t ge vs) (zeroIndicatorIn t v ge)

-- zeroIndicatorIn: zero out the indicator of v in t.
zeroIndicatorIn :: Algebra t p g s e c => t -> t -> (g,e) -> [(g,e)]
zeroIndicatorIn t v ge
  | indicator t v == Just 0 = [ge]
  | indicator t v == Nothing = [ge] -- shouldn't happen
  | otherwise =
    match t target ge
  where
    target = case destroyer v of
      Just s -> substitute s t
      Nothing -> assertError ("Couldn't destroy restricted variable in zeroIndicatorIn " ++ show v ++ " " ++ show t)
