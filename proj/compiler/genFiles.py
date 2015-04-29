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

gen_c = ["./bin/c0c", "-v", "-l", "15411c1.h0", inputFile]

def flush_caches():
    print "Flushing cache..."
    ret = subprocess.call("./flush_cache", shell=True)
    print ret


def genExec(has_opt):
    global gen_c
    global exec_name
    global outFile

    name = exec_name
    if has_opt:
        gen_c += ["--optimization=2"]
        name += "_opt"
    else:
        gen_c += ["--optimization=0"]

    # Make sure to remove old versions of the file
    if os.path.exists(outFile):
        print "Removing old out file"
        os.unlink(outFile)
    if os.path.exists(name):
        print "Removing old exec"
        os.unlink(name)

    subprocess.call(gen_c)

    print "Generating ", name
    gen_exec = ["gcc", "-O3", outFile, "15411c1.c",
                "-o", name]
    subprocess.call(gen_exec)

genExec(True)
genExec(False)
