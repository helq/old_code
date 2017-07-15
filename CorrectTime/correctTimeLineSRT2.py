#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# by helq

from sys import argv

help_message = "correctSRT <file-srt-in> <file-srt-out> <delay> [<caption>]"

if __name__ == "__main__":
    if len(argv) < 4:
        print help_message
        exit(1)

    f_in    =       argv[1]
    f_out   =       argv[2]
    delay   = float(argv[3])
    caption =   int(argv[4]) if len(argv)>4 else 0

    print "input file: ", f_in
    print "output file:", f_out
    print "delay:      ", delay
    print "from caption #"+ str(caption)

    srt = SRTObject( open(f_in, 'r').read() )
    srt.addDelay(delay)

    open(f_out, 'w').write( str(srt) )
