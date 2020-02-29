import os
import subprocess
from subprocess import Popen, PIPE

with open('firefox-js-tests.txt', 'w+') as file:
    def pipe_out(cmd, err=PIPE):
        out = subprocess.Popen(cmd.split(' '), stdout=PIPE, stderr=err)
        file.write(out.stdout.read())

    def tee(s):
        print(s)
        file.write(s)

    owd = os.getcwd()
    os.chdir("../../proofmonkey-gecko-dev")
    tee("Running JSAPI Tests...")
    pipe_out("./mach jsapi-tests")
    tee("Running JIT-Tests...")
    pipe_out("./mach jit-test")
    tee("Running JS Tests... (takes ~30min)\n")
    pipe_out("./mach jstests")

    os.chdir(owd)
    print("Results written to firefox-js-tests.txt")
