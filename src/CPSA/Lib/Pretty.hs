-- A simple pretty printer.

-- The alogithm is by Lawrence C. Paulson, who simplified an algorithm
-- by Derek C. Oppen.

-- Derek C. Oppen, Prettyprinting, ACM Transactions on Programming
-- Languages and Systems, Vol 2, No. 4, October 1980, Pages 465-483.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

{-- A pretty printer based on ML programs with the following copyright

(**** ML Programs from Chapter 8 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)

--}

module CPSA.Lib.Pretty (Pretty, pr, str, brk, blo, grp) where

data Pretty
    = Str !String
    | Brk !Int                  -- Int is the number of breakable spaces
    | Blo ![Pretty] !Int !Int   -- First int is the indent, second int
    --  is the number of chars and spaces for strings and breaks in block
    | Grp ![Pretty] !Int !Int   -- As above

-- Constructors

-- Strings
str :: String -> Pretty
str = Str

-- Break points
brk :: Int -> Pretty
brk = Brk

-- Indentation blocks
-- If the line is too long, not all breaks must be used
blo :: Int -> [Pretty] -> Pretty
blo indent es =
    Blo es indent (len es 0)

-- Indentation groups
-- If the line is too long, all breaks are used
grp :: Int -> [Pretty] -> Pretty
grp indent es =
    Grp es indent (len es 0)

len :: [Pretty] -> Int -> Int
len [] k = k
len (e:es) k = len es (size e + k)

size :: Pretty -> Int
size (Str s) = length s
size (Brk n) = n
size (Blo _ _ n) = n
size (Grp _ _ n) = n

-- Pretty prints the constructed object

pr :: Int -> Pretty -> ShowS
pr margin e s =
    s1
    where
      (_, s1) = printing margin [e] margin 0 False (margin, s)

-- The state of the computation is maintained as a pair consisting of
-- an integer and a string.  The integer is the number of unused
-- character positions in the current line of output.  The printer
-- adds content to the front of the given string.

printing :: Int -> [Pretty] -> Int -> Int -> Bool ->
            (Int, String) -> (Int, String)
printing _ [] _ _ _ p = p
printing margin (e:es) blockspace after force (space, s) =
    (space1, s1)
    where
      (space2, s1) =            -- Result of first item
          case e of
            Str str ->          -- Place a string
                 (space - length str, showString str s2)
            Brk n ->            -- Place breakable space
                 if not force && n + breakdist es after <= space then
                     blanks n (space, s2) -- Don't break
                 else
                     (space3, showChar '\n' s3) -- Break
                     where
                       (space3, s3) =
                           blanks (margin - blockspace) (margin, s2)
            Blo bes indent _ -> -- Place a block
                 printing margin bes (space - indent)
                    (breakdist es after) False (space, s2)
            Grp bes indent n -> -- Place a group
                 printing margin bes (space - indent)
                    dist (n + dist > space) (space, s2)
                where
                  dist = breakdist es after
      (space1, s2) =            -- Result of the remaining items
          printing margin es blockspace after force (space2, s)

-- Find the distance to the nearest breakable space.
breakdist :: [Pretty] -> Int -> Int
breakdist (Str s : es) after = length s + breakdist es after
breakdist (Brk _ : _) _ = 0
breakdist (Blo _ _ n : es) after = n + breakdist es after
breakdist (Grp _ _ n : es) after = n + breakdist es after
breakdist [] after = after

-- Place spaces
blanks :: Int -> (Int, String) -> (Int, String)
blanks n (space, s)
    | n <= 0 = (space, s)
    | otherwise = blanks (n - 1) (space - 1, showChar ' ' s)
