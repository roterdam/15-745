#!/usr/bin/python

import os, sys, subprocess
import time

def print_usage():
    print "python time.py <path_to_input_file> "

args = sys.argv
if (len(args) != 2):
    print_usage()
    sys.exit(1);

inPath = args[1]
(inDir, inFileName) = os.path.split(inPath)
outPath = os.path.join("test_out", ".".join(inFileName.split(".")[:-1]))


def flush_caches():
    print "Flushing cache..."
    ret = subprocess.call("./flush_cache", shell=True)
    print ret


def genExec(has_opt):

    path = (outPath + "_opt") if has_opt else outPath 

    cFilePath = path + ".c"
    gen_c = ["./bin/c0c", "-v", "-l", "15411c1.h0",
             "-O2" if has_opt else "-O0", inPath, "-o", cFilePath]
    if os.path.exists(cFilePath):
        print "rm %s" % cFilePath
        os.unlink(cFilePath)
    print " ".join(gen_c)
    subprocess.call(gen_c)

    execPath = path
    gen_exec = ["gcc", "-O3", cFilePath, "15411c1.c", "-o", execPath]
    if os.path.exists(execPath):
        print "rm %s" % execPath
        os.unlink(execPath)
    print " ".join(gen_exec)
    subprocess.call(gen_exec)

genExec(True)
genExec(False)
