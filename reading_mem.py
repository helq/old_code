#! /usr/bin/env python2
"""
Small script that reads the contents of a process searching for a pattern and replaces it
for a string of the same size. Basically, modifying memory locations of a file.
"""

from __future__ import print_function

import re

process = '383074'
tofind = 'Input pattern'
torep = b'OUT pattern--' # must have the same number of characters
maps_file = open("/proc/{}/maps".format(process), 'r')
mem_file = open("/proc/{}/mem".format(process), 'r', 0)

for line in maps_file.readlines():  # for each mapped region
    m = re.match(r'([0-9A-Fa-f]+)-([0-9A-Fa-f]+) ([-r])', line)
    if m and m.group(3) == 'r':  # if this is a readable region
        start = int(m.group(1), 16)
        end = int(m.group(2), 16)
        mem_file.seek(start)  # seek to region start
        try:
            chunk = mem_file.read(end - start)  # read region contents
        except IOError:
            pass
        pos = re.search(tofind, chunk)
        if pos:
            print(line)
            mem_file_w = open("/proc/{}/mem".format(process), 'wb', 0)
            mem_file_w.seek(start + pos.start())
            mem_file_w.write(torep)
            # exit(1)
        # print(chunk, end="")  # dump contents to standard output

mem_file.close()
maps_file.close()
