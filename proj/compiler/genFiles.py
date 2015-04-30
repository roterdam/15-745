#!/usr/bin/python

import os, sys, subprocess
import time

def print_usage():
    print "python time.py <path_to_input_file> "

args = sys.argv
if (len(args) != 2):
    print_usage()
    sys.exit(1);

inputFile = args[1]
outlist = inputFile.split(".")
outlist.pop(1)
outFile = "".join(outlist + [".c"])
exec_name = outlist[0]


def flush_caches():
    print "Flushing cache..."
    ret = subprocess.call("./flush_cache", shell=True)
    print ret


def genExec(has_opt):
    global exec_name
    global outFile
    gen_c = ["./bin/c0c", "-v", "-l", "15411c1.h0", inputFile]

    name = exec_name
    if has_opt:
        gen_c += ["-O2"]
        name += "_opt"
    else:
        gen_c += ["-O0"]

    # Make sure to remove old versions of the file
    if os.path.exists(outFile):
        print "rm %s" % outFile
        os.unlink(outFile)
    if os.path.exists(name):
        print "rm %s" % name
        os.unlink(name)

    print " ".join(gen_c)
    subprocess.call(gen_c)

    gen_exec = ["gcc", "-O3", outFile, "15411c1.c",
                "-o", name]
    print " ".join(gen_exec)
    subprocess.call(gen_exec)

genExec(True)
genExec(False)
