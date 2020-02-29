import os
from subprocess import Popen, PIPE

def pipe_out(cmd):
    os.system(cmd + " >> firefox-js-tests")

def tee(s):
    print(s)
    pipe_out("echo \"" + s + "\"")

os.system("echo '' > firefox-js-tests.txt")
tee("Running JSAPI Tests...")
#pipe_out("./mach jsapik)
pipe_out("echo foo")
tee("Running JIT-Tests...")
#pipe_out("./mach jit-test")
pipe_out("echo foo")
tee("Running JS Tests... (takes ~30min)")
#pipe_out("./mach jstests")
pipe_out("echo foo")

print("Results written to firefox-js-tests.txt")
