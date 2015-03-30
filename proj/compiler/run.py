#!/usr/bin/python

def printUsageInfo(progName):
  print "usage: %s [-c] <inputFile.c0> [-o <outputFile>]" % progName

def stripExtension(pathName):
  if ("." in pathName):
    return ".".join(pathName.split(".")[:-1])
  return pathName

import os, sys, subprocess

args = sys.argv
assert(len(args) > 0)
if (len(args) == 1):
  printUsageInfo(args[0])
  sys.exit(1)

emitC = False
if (args[1] == "-c"):
  emitC = True
  args.pop(1)

if (len(args) == 1):
  printUsageInfo(args[0])
  sys.exit(1)

inputFile = args.pop(1)
if (not os.path.exists(inputFile)):
  print "Error: bad filename '%s'" % inputFile
  printUsageInfo(args[0])
  sys.exit(1)

if (len(args) == 3 and args[1] == "-o"):
  outputFile = args[2]
elif (len(args) == 1):
  outputFile = stripExtension(inputFile) + (".c" if emitC else "")
else:
  printUsageInfo(args[0])
  sys.exit(1)

cmd = "./bin/c0c -l 15411c1.h0 %s %s -o %s" % ("-c" if emitC else "", inputFile,
                                               outputFile)
os.system(cmd)
