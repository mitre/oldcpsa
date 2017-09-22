# Reader for JSON produced by CPSA's pretty printer program.
# John D. Ramsdell -- August 2014

import json

# Read a file containing a sequence of json objects produced by cpsa's
# pretty printer.  Assumes each object is terminated by a blank line.
def load(f):
    xs = []                     # List of objects read
    buf = ""
    while True:
        l = f.readline()
        if l == "":             # End of file
            if len(buf) > 0:    # Extract JSON object from buf
                xs.append(json.loads(buf))
            return xs
        elif l == "\n":         # Blank line
            if len(buf) > 0:    # Extract JSON object from buf
                xs.append(json.loads(buf))
                buf = ""
        else:
            buf += l            # Append line to buf
