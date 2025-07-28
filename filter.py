#!/usr/bin/env python

import sys
import re

file = open("log.txt", "w")

for line in sys.stdin:
    if line == "\n":
        print()
        continue

    s = [x for x in line.split("\t") if x != ""]
    if len(s) > 1:
        print(s[0], end="")

    if len(s) > 0:
        file.write(s[-1])
