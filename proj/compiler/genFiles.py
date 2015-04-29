import os, sys, subprocess
import time

def print_usage():
    print "python time.py <path_to_input_file> <num_trials>"

args = sys.argv
if (len(args) != 3):
    print_usage()
    sys.exit(1);

num_trials = int(args[2])

inputFile = args[1]
outlist = inputFile.split(".")
outlist.pop(1)
outFile = "".join(outlist + [".c"])
exec_name = outlist[0]

# Make sure to remove old versions of the file
if os.path.exists(outFile):
    print "Removing old out file"
    os.unlink(outFile)
if os.path.exists(outFile):
    print "Removing old exec"
    os.unlink(exec_name)

gen_c = ["./bin/c0c", "-v", "-l", "15411c1.h0", inputFile]

def flush_caches():
    print "Flushing cache..."
    ret = subprocess.call("./flush_cache", shell=True)
    print ret


def time_test_run(has_opt):
    global gen_c
    global num_trials

    if has_opt:
        gen_c += ["--optimization=2"]
    else:
        gen_c += ["--optimization=0"]

    subprocess.call(gen_c)

    gen_exec = ["gcc", "-O0", outFile, "15411c1.c",
                "-o", exec_name]
    subprocess.call(gen_exec)

    total_time = 0
    for i in xrange(num_trials):
        flush_caches()
        start_time = time.time()
        subprocess.call(exec_name)
        end_time = time.time()

        total_time += (end_time - start_time)

    os.unlink(outFile)
    os.unlink(exec_name)

    return total_time/num_trials

seconds_taken_without_opt = time_test_run(False)
print "Time taken without opt: %fs" % seconds_taken_without_opt
flush_caches()
seconds_taken_with_opt = time_test_run(True)
print "Time taken with opt: %fs" % seconds_taken_with_opt
print "Speedup: %f" % (seconds_taken_without_opt/seconds_taken_with_opt)
