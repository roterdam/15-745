#!/usr/bin/python

def printUsageInfo(progName):
  print "usage: %s [-c0|-ast|-c] <inputFile.c0> [-o <outputFile>]" % progName

def stripExtension(pathName):
  if ("." in pathName):
    return ".".join(pathName.split(".")[:-1])
  return pathName

def extensionForEmitFlag(flag):
  if (flag == "-c0"):
    return ".c0"
  elif (flag == "-ast"):
    return ".ast"
  elif (flag == "-c"):
    return ".c"
  else:
    assert(False)

import os, sys, subprocess

args = sys.argv
assert(len(args) > 0)
if (len(args) == 1):
  printUsageInfo(args[0])
  sys.exit(1)

emitFlag = "-c"
if (args[1] in ["-c0", "-ast", "-c"]):
  emitFlag = args.pop(1)

if (len(args) == 1):
  printUsageInfo(args[0])
  sys.exit(1)

inputPath = args.pop(1)
if (not os.path.exists(inputPath)):
  print "Error: bad filename '%s'" % inputPath
  printUsageInfo(args[0])
  sys.exit(1)

if (len(args) == 3 and args[1] == "-o"):
  outputFile = args[2]
elif (len(args) == 1):
  (rest, fileName) = os.path.split(inputPath)
  newfileName = "_" + stripExtension(fileName) + extensionForEmitFlag(emitFlag)
  outputFile = os.path.join(rest, newfileName)
  if (os.path.exists(outputFile)):
    print "Error: output file '%s' already exists" % outputFile
    sys.exit(1)
else:
  printUsageInfo(args[0])
  sys.exit(1)

cmd = "./bin/c0c -v -l 15411c1.h0 -%s %s -o %s" % (emitFlag, inputPath, outputFile)
print cmd
os.system(cmd)
