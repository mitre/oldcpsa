-- Exports the types and functions used to construct an implementation
-- of an algebra.

-- Each algebra provides an instance of each type class in the Algebra
-- module, and exports its name, and its starting variable generator.

-- module CPSA.????.Loader (name, origin) where
-- import qualified CPSA.Lib.CPSA as C
-- import CPSA.Lib.CPSA (SExpr(..), Pos, annotation)

-- The algebra is made available for use by adding the name origin
-- pair to the association list in CPSA.Lib.Main.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.CPSA (module CPSA.Lib.Utilities,
                      module CPSA.Lib.SExpr,
                      module CPSA.Lib.Pretty,
                      module CPSA.Lib.Printer,
                      module CPSA.Lib.Notation,
                      module CPSA.Lib.Algebra) where

import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Pretty
import CPSA.Lib.Printer
import CPSA.Lib.Notation
import CPSA.Lib.Algebra
