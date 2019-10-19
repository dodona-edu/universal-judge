
import sys

test_file = open("output.txt", "w")


sys.stderr.writeln("--test--")
sys.stdout.writeln("--test--")

from .submission import *

sys.stderr.writeln("--test--")
sys.stdout.writeln("--test--")