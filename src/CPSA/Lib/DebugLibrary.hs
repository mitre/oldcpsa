-- Debugging functions

-- Copyright (c) 2013 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.DebugLibrary where

import CPSA.Lib.Algebra
import CPSA.Lib.Protocol
import CPSA.Lib.Strand
import qualified Data.Set as S
import Data.Set(Set)

zi :: Algebra t p g s e c => Instance t p g s e c -> String
zi inst =
    show (map f e)
    where
      domain = rvars (role inst)
      e = reify domain (env inst)
      range = map snd e
      f (x, t) = (displayTerm (context domain) x,
                  displayTerm (context range) t)
      context ts = addToContext emptyContext ts

zs :: Algebra t p g s e c => Set t -> String
zs s =
    show $ map (displayTerm (addToContext emptyContext ts)) ts
    where
      ts = S.toList s
