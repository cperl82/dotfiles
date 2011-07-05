import os
import sys

from datetime import datetime, date, time
from pprint import pprint as pp

def hexdump(data):
    offset = 0
    while offset < len(data):
        col1  = data[offset:offset+8]
        col2  = data[offset+8:offset+16]
        col1  = [ ord(x) for x in col1 ]
        col2  = [ ord(x) for x in col2 ]
        ascii = [ chr(x) if 31 < x < 127 else '.' for x in col1 + col2 ]
        fmt = "{0:08x}  {1:23}  {2:23}  |{3:16}|"
        s =  fmt.format(offset, 
                        ' '.join([ "{0:02x}".format(x) for x in col1]),
                        ' '.join([ "{0:02x}".format(x) for x in col2]),
                        ''.join(ascii))
        print s
        offset = offset + 16
