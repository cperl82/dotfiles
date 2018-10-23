#!/usr/bin/env python

import os
import sys
import struct
import base64

# Decoding accoriding to http://blogs.msdn.com/b/oldnewthing/archive/2004/03/15/89753.aspx

def main():
    data = sys.stdin.read()
    data = base64.b64decode(data)
    rev = struct.unpack("B", data[:1])[0]
    ndash = struct.unpack("B", data[1:2])[0]
    a = struct.unpack(">H", data[2:4])[0]
    b = struct.unpack(">L", data[4:8])[0]
    c = (a << 32) | b
    i = 8
    n = ""
    for dash in range(ndash):
        d = struct.unpack("<L", data[i:i+4])[0]
        n = "%s-%d" % (n, d)
        i += 4
    print "S-%d-%d%s" % (rev, c, n) 


if __name__ == "__main__":
    main()
