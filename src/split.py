#!/usr/bin/env python3

# Skeleton split
# John D. Ramsdell -- August 2014

# Copies the skeletons in a CPSA source file into separate files.
#
# If the source filename is root.json, it creates file root_N.json,
# where root_N.json contains the nth skeleton in the source file.
#
# Use cpsapp -j to translate the source file into JSON.  Use cpsajson
# to translate the output files into CPSA S-Expressions.

import sys
import os.path
import json
import cpsajson

def main():
    if len(sys.argv) != 2:      # Ensure one file name supplied
        print("Usage: " + sys.argv[0] + " file")
    fname = sys.argv[1]
    (root, ext) = os.path.splitext(fname)
    f = open(fname, 'r')
    xs = cpsajson.load(f)
    f.close()
    i = 0                       # Skeleton number
    pd = {}                     # Protocol dictionary
    h = []                      # Herald form
    for x in xs:
        if x[0] == "herald":
            h = x               # Store herald
        elif x[0] == "defprotocol":
            pd[x[1]] = x        # Store protocol
        elif x[0] == "defskeleton":
            p = pd[x[1]]        # Look up protocol
            f = open(root + "_" + str(i) + ext, 'w')
            i += 1
            if h:               # Dump herald (maybe)
                json.dump(h, f)
            json.dump(p, f)     # Dump protocol
            json.dump(x, f)     # Dump skeleton
            f.close()

if __name__ == "__main__":
    main()
